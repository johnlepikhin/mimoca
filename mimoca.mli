
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

val of_channel: Lwt_io.input_channel -> t Lwt.t

val decoded_header: header list -> string -> string
