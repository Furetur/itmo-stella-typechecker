let choose_type_model exts : Type_model.is_subtype_checker =
  let open Extentions in
  if is_structural_subtyping_enabled exts then (
    Logs.debug (fun m -> m "Type model: Structural_subtyping_model");
    Structural_type_model.is_subtype)
  else (
    Logs.debug (fun m -> m "Type model: Syntax_equality_type_model");
    Default_type_model.is_subtype)
