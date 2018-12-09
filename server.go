package main

import (
	"compress/gzip"
	"io"
	"log"
	"net/http"
	"os"
	"strings"

	"github.com/gorilla/mux"
)

const (
	sewingPatternEditorDistPath = "SEWING_PATTERN_EDITOR_DIST_PATH"
	sewingPatternEditorPort     = "SEWING_PATTERN_EDITOR_PORT"
)

func main() {
	distPath := os.Getenv(sewingPatternEditorDistPath)
	if distPath == "" {
		log.Fatal(sewingPatternEditorDistPath + " must be set")
	}

	port := os.Getenv(sewingPatternEditorPort)
	if port == "" {
		log.Fatal(sewingPatternEditorPort + " must be set")
	}

	r := mux.NewRouter()

	r.Methods("GET").PathPrefix("/").Handler(http.FileServer(http.Dir(distPath)))

	http.Handle("/", r)
	log.Println("Listening on port " + port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
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
