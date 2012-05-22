
let int_of_hex b =
	let cc = Char.code b in
	if cc > 96 then
		cc - 87
	else if cc > 64 then
		cc - 55
	else
		cc - 48

let decode s =
	let len = String.length s in
	let r = Buffer.create len in
	let rec loop pos =
		if pos = len then
			()
		else
			if s.[pos] = '=' then
				begin
					if s.[pos+1] = '\n' then
						loop (pos+2)
					else
					begin
						let b1 = int_of_hex (s.[pos+1]) in
						let b2 = int_of_hex (s.[pos+2]) in
						Buffer.add_char r (Char.chr ((b1 lsl 4) + b2));
						loop (pos+3)
					end
				end
			else
			begin
				Buffer.add_char r s.[pos];
				loop (pos+1)
			end
	in
	loop 0;
	Buffer.contents r

