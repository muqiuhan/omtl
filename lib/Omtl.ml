let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "==> Execution time: %f secondsn\n\n" (Unix.gettimeofday () -. t);
  res
;;

