
let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let echar = '='

let decode s =
	let l = String.length s in
	let r = Buffer.create l in
	let rec drop buf = function
		| -1 -> 0
		| n ->
			let b = (buf lsr (n*8)) land 0b11111111 in
			Buffer.add_char r (Char.chr b);
			drop buf (n-1)
	in
	let rec loop buf pos b64pos =
		if pos = l then
			()
		else
			let c = s.[pos] in
			if c = echar then
				ignore (drop buf 2)
			else
			begin
				if c = '\n' then
					loop buf (pos+1) b64pos
				else
				begin
					let bpos = b64pos mod 4 in
					let buf = buf lor ((String.index chars c) lsl (18-bpos*6)) in
					if bpos = 3 then
						loop (drop buf 2) (pos+1) (b64pos+1)
					else
						loop buf (pos+1) (b64pos+1)
				end
			end
	in
	loop 0 0 0;
	Buffer.contents r

let encode s =
	let l = String.length s in
	let r = Buffer.create (l*2) in
	let rec drop buf = function
		| 4 -> 0
		| n ->
			let c = chars.[(buf lsr (6*(3-n))) land 0b111111] in
			Buffer.add_char r c;
			drop buf (n+1)
	in
	let rec loop buf pos =
		let bpos = pos mod 3 in
		if pos < l then
		begin
			let buf = buf + ((Char.code s.[pos]) lsl ((2-bpos) * 8)) in
			if bpos = 2 then
				loop (drop buf 0) (pos+1)
			else
				loop buf (pos+1)
		end
		else
			if bpos > 0 then
			begin
				for i=0 to bpos do
					let c = chars.[(buf lsr (6*(3-i))) land 0b111111] in
					Buffer.add_char r c;
				done;
				Buffer.add_string r (String.make (3-bpos) echar)
			end
			else ()
	in
	loop 0 0;
	Buffer.contents r

let encode_body =
	let limit = 78 in
	fun s ->
		let s = encode s in
		let l = String.length s in
		let r = Buffer.create l in
		for i=0 to l/limit do
			let pos = i*limit in
			let len = if pos + limit < l then limit else l-pos in
			Buffer.add_substring r s (i*limit) len;
			Buffer.add_char r '\n';
		done;
		Buffer.contents r

