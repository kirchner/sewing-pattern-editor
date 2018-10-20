package main

import (
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()

	r.Methods("GET").Path("/assets/js/elm.js").HandlerFunc(serveElmAsset)
	r.Methods("GET").PathPrefix("/assets/").Handler(
		http.StripPrefix("/assets/",
			http.FileServer(http.Dir("assets"))))
	r.Methods("GET").PathPrefix("/").HandlerFunc(serveApplication)

	http.Handle("/", r)
	log.Println("Listening on port 8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

func serveElmAsset(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/javascript")
	http.ServeFile(w, r, "assets/js/elm.js")
}

func serveApplication(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, "src/index.html")
}
