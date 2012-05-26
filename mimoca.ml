
type header = string * string

type content =
	| Text of string
	| Image of (string * string * string)

type entry =
	| Content of content
	| Parts of t list
	| Unknown of string

and t = {
	headers : header list;
	content_type : string;
	entry : entry;
}

module ContentType = struct
	type t =
		| Text of string * string
		| Image of string * string
		| MultiPart of string * string
		| Unknown of string
end

let get_headers n l =
	let n = String.lowercase n in
	let l = List.find_all (fun (hn, hv) -> String.lowercase hn = n) l in
	List.map (fun (_, v) -> v) l

let get_header n l =
	match get_headers n l with
		| hd :: _ -> hd
		| [] -> raise Not_found

let read_headers =
	let rex_hd = Pcre.regexp "^([a-zA-Z0-9_-]+):\\s+(.*)" in
	let rex_tl = Pcre.regexp "^\\s+(.*)" in
	fun ch ->
		let r = ref [] in
		let name = ref "" in
		let v = Buffer.create 1000 in
		let commit () =
			if String.length !name > 0 then
			begin
				r := (!name, Buffer.contents v) :: !r;
				name := "";
				Buffer.clear v;
				Lwt.return ()
			end
			else
				Lwt.return ()
		in
		let rec loop () =
			lwt line = Lwt_io.read_line ch in
			if String.length line = 0 then
				commit ()
			else
			begin
				try
					let sub = Pcre.exec ~rex:rex_hd line in
					lwt _ = commit () in
					let hn = Pcre.get_substring sub 1 in
					let hv = Pcre.get_substring sub 2 in
					name := hn;
					Buffer.clear v;
					Buffer.add_string v hv;
					loop ()
				with
					| _ ->
						let sub = Pcre.exec ~rex:rex_tl line in
						let hv = Pcre.get_substring sub 1 in
						Buffer.add_char v '\n';
						Buffer.add_string v hv;
						loop ()
			end
		in
		lwt _ = loop () in
		let r = List.rev !r in
		Lwt.return r

let read_body ch =
	let r = Buffer.create 4000 in
	let rec loop () =
		try_lwt
			lwt line = Lwt_io.read_line ch in
			Buffer.add_string r line;
			Buffer.add_char r '\n';
			loop ()
		with
			| _ ->
				Lwt.return ()
	in
	lwt _ = loop () in
	let r = Buffer.contents r in
	Lwt.return r

let transfer_decode hdrs body =
	let enc = get_header "Content-Transfer-Encoding" hdrs in
	let decode =
		match String.lowercase enc with
			| "base64" -> Base64.decode
			| "quoted-printable" -> Qprintable.decode
			| enc -> raise (failwith ("Unknown transfer encoding: " ^ enc))
	in
	decode body

let get_content_type =
	let lst = [
		(Pcre.regexp ~flags:[`CASELESS] "(text/[^;]+);\\s+charset=\"?([a-zA-Z0-9_-]+)\"?"),
		(fun s1 s2 ->
			let s2 = String.lowercase s2 in
			ContentType.Text (s1, s2)
		);
		(Pcre.regexp ~flags:[`CASELESS] "(image/[^;]+);\\s+name=\"?([^\"]+)\"?"),
		(fun s1 s2 ->
			ContentType.Image (s1, s2)
		);
		(Pcre.regexp ~flags:[`CASELESS] "(multipart/[^;]+);\\s+boundary=\"?([^\"]+)\"?"),
		(fun s1 s2 ->
			ContentType.MultiPart (s1, s2)
		);
		(Pcre.regexp "([^;]+)(.*)"),
		(fun s1 s2 ->
			ContentType.Unknown s1
		);
	] in
	fun hdrs ->
		try
			let ctype = get_header "content-type" hdrs in
			let rec loop = function
				| [] -> ContentType.Unknown ""
				| (rex, f) :: tl ->
					try
						let sub = Pcre.exec ~rex ctype in
						let s1 = Pcre.get_substring sub 1 in
						let s2 = Pcre.get_substring sub 2 in
						let s1 = String.lowercase s1 in
						f s1 s2
					with
						| _ ->
							loop tl
			in
			loop lst
		with
			| _ ->
				raise (failwith "Cannot get Content-Type")

let utf8_of_string charset s =
	let decode =
		match charset with
			| "cp1251"
			| "windows-1251" ->
				Charset_cp1251.string_to_utf8
			| "koi8-r" ->
				Charset_koi8r.string_to_utf8
			| "utf8"
			| "utf-8"
			| "iso-8859-1"
			| "latin1" ->
				(fun s -> s)
			| c ->
				raise (failwith ("Unknown charset: " ^ c))
	in
	decode s

let decoded_header =
	let rex = Pcre.regexp "^=\\?([^\\?]+)\\?(.)\\?([^\\?]+)\\?=" in
	let split_rex = Pcre.regexp "\n" in
	fun hdrs name ->
		let r = Buffer.create 4000 in
		let rec loop = function
			| [] -> ()
			| hd :: tl ->
				let part =
					try
						let sub = Pcre.exec ~rex hd in
						let charset = Pcre.get_substring sub 1 in
						let enc = Pcre.get_substring sub 2 in
						let cont = Pcre.get_substring sub 3 in
						let charset = String.lowercase charset in
						let enc = String.lowercase enc in
						let decode =
						match enc with
								| "b" -> Base64.decode
								| "q" -> Qprintable.decode
								| _ -> raise (failwith ("Unknown header encoder: " ^ enc))
						in
						let cont = decode cont in
						utf8_of_string charset cont
					with
						| _ -> hd
				in
				Buffer.add_string r part;
				loop tl
		in
		let v = get_header name hdrs in
		let lst = Pcre.split ~rex:split_rex v in
		loop lst;
		Buffer.contents r

let get_parts ch boundary =
	let boundary = "--" ^ boundary in
	let boundary_end = boundary ^ "--" in
	let r = ref [] in
	let b = Buffer.create 4000 in
	let is_inside = ref false in
	let commit () =
		r := (Buffer.contents b) :: !r;
		Buffer.clear b
	in
	let rec loop () =
		try_lwt
			lwt line = Lwt_io.read_line ch in
			if line = boundary then
				if !is_inside then
				begin
					commit ();
					loop ()
				end
				else
				begin
					is_inside := true;
					loop ()
				end
			else
			if line = boundary_end then
			begin
				commit ();
				Lwt.return ()
			end
			else
				if !is_inside then
				begin
					Buffer.add_string b line;
					Buffer.add_char b '\n';
					loop ()
				end
				else
					loop ()
		with
			| _ ->
				if !is_inside then
					Lwt.fail (failwith "Unexpected end of part")
				else
					Lwt.return ()
	in
	lwt _ = loop () in
	let r = List.rev !r in
	Lwt.return r

let rec of_channel ch =
	lwt hdrs = read_headers ch in
	lwt body = read_body ch in
	let content_type = get_content_type hdrs in
	match content_type with
		| ContentType.Text (ctype, charset) ->
			let body = transfer_decode hdrs body in
			let body = utf8_of_string charset body in
			let r = {
				headers = hdrs;
				content_type = ctype;
				entry = Content (Text body);
			} in
			Lwt.return r
		| ContentType.Image (ctype, name) ->
			let body = transfer_decode hdrs body in
			let r = {
				headers = hdrs;
				content_type = ctype;
				entry = Content (Image (ctype, body, name));
			} in
			Lwt.return r
		| ContentType.MultiPart (ctype, boundary) ->
			let bytes = Lwt_bytes.of_string body in
			let ch = Lwt_io.of_bytes ~mode:Lwt_io.input bytes in
			lwt parts = get_parts ch boundary in
			lwt parts = Lwt_list.map_s (fun part ->
				let bytes = Lwt_bytes.of_string part in
				let ch = Lwt_io.of_bytes ~mode:Lwt_io.input bytes in
				of_channel ch
			) parts in
			let r = {
				headers = hdrs;
				content_type = ctype;
				entry = Parts parts;
			} in
			Lwt.return r
		| ContentType.Unknown c ->
			let r = {
				headers = hdrs;
				content_type = c;
				entry = Unknown body;
			} in
			Lwt.return r
