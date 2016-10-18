
type blt_ctx

val create_context : unit -> blt_ctx
val handle_line : string -> blt_ctx -> blt_ctx
val check_consistency : blt_ctx -> unit
