[@@@warning "-27-32"]

type ptype = TNormal | TFire | TWater
type mon = { name : string; hp : int; ptype : ptype }

let charmander = { name = "Charmander"; hp = 39; ptype = TFire }
let get_hp m = match m with { name = n; hp = h; ptype = p } -> h
let get_hp m = match m with { name = _; hp = h; ptype = _ } -> h
let get_hp m = match m with { name; hp; ptype } -> hp
let get_hp m = match m with { hp; _ } -> hp
let get_hp m = m.hp
