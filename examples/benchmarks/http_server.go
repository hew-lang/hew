package main

import (
	"fmt"
	"net/http"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/":
			w.WriteHeader(200)
			fmt.Fprint(w, "Hello from Go!")
		case "/health":
			w.WriteHeader(200)
			fmt.Fprint(w, "OK")
		default:
			w.WriteHeader(404)
			fmt.Fprint(w, "Not Found")
		}
	})
	fmt.Println("Listening on 0.0.0.0:18081")
	http.ListenAndServe("0.0.0.0:18081", nil)
}
