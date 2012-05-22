open Ocamlbuild_plugin

let files = ["META"; "mimoca.cmxa"; "mimoca.cma"; "mimoca.mli"; "mimoca.a"; "mimoca.cmi"]

let syntaxes = [ "camlp4o"; "camlp4r"; "camlp5o"; "camlp5r" ]

let flag_all_except_link tag f =
	flag ["ocaml"; "compile"; tag] f;
	flag ["ocaml"; "ocamldep"; tag] f;
	flag ["ocaml"; "doc"; tag] f

let rule_ocamlfind l _ _ = Cmd (S((A"ocamlfind") :: l))

let installer_rules ~files ~name =
	let deps = List.map (fun f -> f) files in
	let files = List.map (fun f -> A f) files in
	rule ("Install " ^ name) ~prod:"install" ~deps (rule_ocamlfind (A"install" :: A name :: files));
	rule ("Uninstall " ^ name) ~prod:"uninstall" ~deps:[] (rule_ocamlfind [A"remove"; A name]);
	rule ("Reinstall" ^ name) ~prod:"reinstall" ~deps:["uninstall"; "install"] (fun _ _ -> Cmd (S[A"/bin/true"]))


let _ =
	dispatch
		begin
			function
				| After_rules ->
					List.iter (fun syntax -> flag_all_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax])) syntaxes;
					installer_rules ~files ~name:"mimoca"
				| _ ->
					()
		end
