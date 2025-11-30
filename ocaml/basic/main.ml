(* OCaml Basic Implementation for Billion Row Challenge *)

open Printf

(* 통계 데이터 구조 *)
type status = {
  mutable min : int64;
  mutable max : int64;
  mutable total : int64;
  mutable count : int64;
}

(* 파일 경로 상수 *)
let output_path = "outputs.txt"
let measurements_path = "measurements.txt"

(* 문자열을 구분자로 분리 *)
let split_string str delimiter =
  try
    let index = String.index str delimiter in
    let first = String.sub str 0 index in
    let second = String.sub str (index + 1) (String.length str - index - 1) in
    Some (first, second)
  with Not_found -> None

(* 솔루션 함수 *)
let solution path =
  let status_map = Hashtbl.create 100 in

  (* 파일을 한 줄씩 읽기 *)
  let ic = open_in path in
  try
    while true do
      let line = input_line ic in

      (* 세미콜론으로 분리 *)
      match split_string line ';' with
      | Some (city_name, measurement_str) ->
          (try
            let measurement = Int64.of_string measurement_str in

            (* HashMap에서 도시 찾기 *)
            if Hashtbl.mem status_map city_name then
              let status = Hashtbl.find status_map city_name in
              status.min <- Int64.min status.min measurement;
              status.max <- Int64.max status.max measurement;
              status.total <- Int64.add status.total measurement;
              status.count <- Int64.add status.count 1L
            else
              Hashtbl.add status_map city_name {
                min = measurement;
                max = measurement;
                total = measurement;
                count = 1L;
              }
          with Failure _ -> ())
      | None -> ()
    done;
    ""
  with End_of_file ->
    close_in ic;

    (* 도시 이름으로 정렬 *)
    let sorted_cities =
      Hashtbl.fold (fun city _ acc -> city :: acc) status_map []
      |> List.sort String.compare
    in

    (* 결과 문자열 생성 *)
    let buffer = Buffer.create 10000 in
    List.iter (fun city_name ->
      let status = Hashtbl.find status_map city_name in
      let avg = Int64.div status.total status.count in
      Buffer.add_string buffer (sprintf "%s=%Ld;%Ld;%Ld(%Ld/%Ld)\n"
        city_name
        status.min
        status.max
        avg
        status.total
        status.count)
    ) sorted_cities;

    Buffer.contents buffer

(* 파일 전체 읽기 *)
let read_file path =
  let ic = open_in path in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  content

(* 메인 함수 *)
let main () =
  (* 예상 출력 읽기 *)
  let expected_output = read_file output_path in

  (* 타이머 시작 *)
  let start_time = Unix.gettimeofday () in
  let result = solution measurements_path in
  let end_time = Unix.gettimeofday () in
  let elapsed_ms = int_of_float ((end_time -. start_time) *. 1000.0) in

  printf "Elapsed: %dms\n" elapsed_ms;

  (* 결과 비교 *)
  if expected_output = result then
    printf "Matched!\n"
  else begin
    printf "Output does not match expected!\n";

    (* 디버깅용 첫 몇 줄 출력 *)
    let expected_lines = String.split_on_char '\n' expected_output in
    let result_lines = String.split_on_char '\n' result in

    printf "Expected first 3 lines:\n";
    List.iteri (fun i line ->
      if i < 3 then printf "  %s\n" line
    ) expected_lines;

    printf "Got first 3 lines:\n";
    List.iteri (fun i line ->
      if i < 3 then printf "  %s\n" line
    ) result_lines
  end

(* 프로그램 시작 *)
let () = main ()
