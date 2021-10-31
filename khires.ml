(* Js_of_ocaml example
    * http://www.ocsigen.org/js_of_ocaml/
    * Copyright (C) 2010 Jérôme Vouillon
    * Laboratoire PPS - CNRS Université Paris Diderot
    *
    * This program is free software; you can redistribute it and/or modify
    * it under the terms of the GNU General Public License as published by
    * the Free Software Foundation; either version 2 of the License, or
    * (at your option) any later version.
    *
    * This program is distributed in the hope that it will be useful,
    * but WITHOUT ANY WARRANTY; without even the implied warranty of
    * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    * GNU Lesser General Public License for more details.
    *
    * You should have received a copy of the GNU Lesser General Public License
    * along with this program; if not, write to the Free Software
    * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)
open Js_of_ocaml
(*open Js_of_ocaml_lwt*)

module Html = Dom_html

let js = Js.string

let document = Html.window##.document

type cell = Flag | Mine | Unknown

and board =
  { 
    rows : int
  ; cols : int
  ; map : cell array array
  ; imgs : Html.imageElement Js.t array array
  }

let img_assoc v =
  match v with
  | Flag -> js "sprites/normal.png"
  | Mine -> js "sprites/empty.png"
  | Unknown -> js "sprites/empty.png"

let set_cell state x y v =
  state.map.(y).(x) <- v;
  state.imgs.(y).(x)##.src := img_assoc v

let draw_cell img cell =
  img##.src := img_assoc cell

let draw_board board=
  for y = 0 to board.rows -1 do
    for x = 0 to board.cols -1 do
      draw_cell board.imgs.(y).(x) board.map.(x).(y)
    done
  done

let create_board col row =
  let total = col * row in
  { rows = row;
    cols = col;
    map = Array.make total [||];
    imgs = Array.make total [||];
  }

let init_table board board_div =
  let buf = document##createDocumentFragment in
  for y = 0 to board.rows -1 do
    let imgs = ref [] in
    for x = 0 to board.cols -1 do
      let img = Html.createImg document in
      print_int x;
      imgs := img :: !imgs;
      img##.src := js "sprites/normal.png";
      img##.onclick := 
        Html.handler (fun (x) -> let button = Js.to_bool x##.ctrlKey in
            (match button with
             | false -> img##.src := js "sprites/flag.png"
             | true -> img##.src := js "sprites/bomb.png"
            );
            Js._false);
      Dom.appendChild buf img
    done;
    Dom.appendChild buf (Html.createBr document);
    board.imgs.(y) <- Array.of_list (List.rev !imgs)
  done;
  board_div##.style##.lineHeight := js "0";
  Dom.appendChild board_div buf

let run div col row =
  let board = create_board col row in
  init_table board div

let onload _ =
  let main = Js.Opt.get (document##getElementById (js "main")) (fun () -> assert false) in
  let div = Html.createDiv document in
  Dom.appendChild main div;
  run div 5 10;
  Js._false

let _ = Html.window##.onload := Html.handler onload

