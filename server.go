package main

import (
	"log"
	"net/http"
	"os"

	"github.com/NYTimes/gziphandler"
	"github.com/gorilla/mux"
)

const (
	distPath   = "dist"
	herokuPort = "PORT"
)

func main() {
	port := os.Getenv(herokuPort)
	if port == "" {
		log.Fatal(herokuPort + " must be set")
	}

	r := mux.NewRouter()

	r.Methods("GET").PathPrefix("/patterns").Handler(gziphandler.GzipHandler(
		serveIndex(distPath)))
	r.Methods("GET").PathPrefix("/measurements").Handler(gziphandler.GzipHandler(
		serveIndex(distPath)))
	r.Methods("GET").PathPrefix("/persons").Handler(gziphandler.GzipHandler(
		serveIndex(distPath)))
	r.Methods("GET").PathPrefix("/editor").Handler(gziphandler.GzipHandler(
		serveIndex(distPath)))

	r.Methods("GET").PathPrefix("/").Handler(gziphandler.GzipHandler(
		http.FileServer(http.Dir(distPath))))

	http.Handle("/", r)

	log.Println("Listening on port " + port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}

func serveIndex(distPath string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, distPath+"/index.html")
	})
}
