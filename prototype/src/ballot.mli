
type t

exception Duplicate_prefs

val create : int -> int array -> t
val total_preferences : t -> int
