let code_to_utf8 = function
	| 127 -> ""
	| 128 -> "Ђ"
	| 129 -> "Ѓ"
	| 130 -> "‚"
	| 131 -> "ѓ"
	| 132 -> "„"
	| 133 -> "…"
	| 134 -> "†"
	| 135 -> "‡"
	| 136 -> "€"
	| 137 -> "‰"
	| 138 -> "Љ"
	| 139 -> "‹"
	| 140 -> "Њ"
	| 141 -> "Ќ"
	| 142 -> "Ћ"
	| 143 -> "Џ"
	| 144 -> "ђ"
	| 145 -> "‘"
	| 146 -> "’"
	| 147 -> "“"
	| 148 -> "”"
	| 149 -> "•"
	| 150 -> "–"
	| 151 -> "—"
	| 153 -> "™"
	| 154 -> "љ"
	| 155 -> "›"
	| 156 -> "њ"
	| 157 -> "ќ"
	| 158 -> "ћ"
	| 159 -> "џ"
	| 160 -> " "
	| 161 -> "Ў"
	| 162 -> "ў"
	| 163 -> "Ј"
	| 164 -> "¤"
	| 165 -> "Ґ"
	| 166 -> "¦"
	| 167 -> "§"
	| 168 -> "Ё"
	| 169 -> "©"
	| 170 -> "Є"
	| 171 -> "«"
	| 172 -> "¬"
	| 173 -> "­"
	| 174 -> "®"
	| 175 -> "Ї"
	| 176 -> "°"
	| 177 -> "±"
	| 178 -> "І"
	| 179 -> "і"
	| 180 -> "ґ"
	| 181 -> "µ"
	| 182 -> "¶"
	| 183 -> "·"
	| 184 -> "ё"
	| 185 -> "№"
	| 186 -> "є"
	| 187 -> "»"
	| 188 -> "ј"
	| 189 -> "Ѕ"
	| 190 -> "ѕ"
	| 191 -> "ї"
	| 192 -> "А"
	| 193 -> "Б"
	| 194 -> "В"
	| 195 -> "Г"
	| 196 -> "Д"
	| 197 -> "Е"
	| 198 -> "Ж"
	| 199 -> "З"
	| 200 -> "И"
	| 201 -> "Й"
	| 202 -> "К"
	| 203 -> "Л"
	| 204 -> "М"
	| 205 -> "Н"
	| 206 -> "О"
	| 207 -> "П"
	| 208 -> "Р"
	| 209 -> "С"
	| 210 -> "Т"
	| 211 -> "У"
	| 212 -> "Ф"
	| 213 -> "Х"
	| 214 -> "Ц"
	| 215 -> "Ч"
	| 216 -> "Ш"
	| 217 -> "Щ"
	| 218 -> "Ъ"
	| 219 -> "Ы"
	| 220 -> "Ь"
	| 221 -> "Э"
	| 222 -> "Ю"
	| 223 -> "Я"
	| 224 -> "а"
	| 225 -> "б"
	| 226 -> "в"
	| 227 -> "г"
	| 228 -> "д"
	| 229 -> "е"
	| 230 -> "ж"
	| 231 -> "з"
	| 232 -> "и"
	| 233 -> "й"
	| 234 -> "к"
	| 235 -> "л"
	| 236 -> "м"
	| 237 -> "н"
	| 238 -> "о"
	| 239 -> "п"
	| 240 -> "р"
	| 241 -> "с"
	| 242 -> "т"
	| 243 -> "у"
	| 244 -> "ф"
	| 245 -> "х"
	| 246 -> "ц"
	| 247 -> "ч"
	| 248 -> "ш"
	| 249 -> "щ"
	| 250 -> "ъ"
	| 251 -> "ы"
	| 252 -> "ь"
	| 253 -> "э"
	| 254 -> "ю"
	| 255 -> "я"
	| c -> String.make 1 (Char.chr c)

let string_to_utf8 s =
	let len = String.length s in
	let b = Buffer.create (len*3) in
	for pos=0 to len-1 do
		Buffer.add_string b (code_to_utf8 (Char.code (s.[pos])));
	done;
	Buffer.contents b
