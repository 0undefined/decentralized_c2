* Get Cpu/Mem info

      watch -t "ps -p $(pgrep beam | tr '\n' ',' | sed -E 's/,$//g') -o %cpu,%mem"

* Get number of all erlang instances

      watch -t "pgrep beam | wc -l"

* Get missing/dead nodes in range

      for i in {01..40}; do ls tmp/test$i* 1>/dev/null; done
