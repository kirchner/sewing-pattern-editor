package main

import (
	"compress/gzip"
	"io"
	"log"
	"net/http"
	"strings"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()

	r.Methods("GET").Path("/elm.js").HandlerFunc(
		serveJSAsset("assets/js/elm"))
	r.Methods("GET").Path("/service-worker.js").HandlerFunc(
		serveJSAsset("assets/js/service-worker"))
	r.Methods("GET").Path("/register-service-worker.js").HandlerFunc(
		serveJSAsset("assets/js/register-service-worker"))

	r.Methods("GET").PathPrefix("/assets/").Handler(
		http.StripPrefix("/assets/",
			http.FileServer(http.Dir("assets"))))
	r.Methods("GET").PathPrefix("/").HandlerFunc(serveApplication)

	http.Handle("/", r)
	log.Println("Listening on port 8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

func serveJSAsset(name string) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/javascript")
		if !strings.Contains(r.Header.Get("Accept-Encoding"), "gzip") {
			http.ServeFile(w, r, name+".js")
			return
		}

		w.Header().Set("Content-Encoding", "gzip")
		gz := gzip.NewWriter(w)
		defer gz.Close()
		gzr := gzipResponseWriter{Writer: gz, ResponseWriter: w}
		http.ServeFile(gzr, r, name+".js")
	}
}

type gzipResponseWriter struct {
	io.Writer
	http.ResponseWriter
}

func (w gzipResponseWriter) Write(b []byte) (int, error) {
	return w.Writer.Write(b)
}

func serveApplication(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, "src/index.html")
}
