
type content =
	| Text of string
	| Image of (string * string)

type entry =
	| Content of content
	| Parts of t list
	| Unknown of string

and t = {
	headers : (string * string) list;
	content_type : string;
	entry : entry;
}

val of_channel: Lwt_io.input_channel -> t Lwt.t
