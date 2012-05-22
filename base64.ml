
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

