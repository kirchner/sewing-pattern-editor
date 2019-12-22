package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
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

	r.Methods("GET").PathPrefix("/static").
		Handler(http.StripPrefix("/static",
			gziphandler.GzipHandler(http.FileServer(http.Dir(distPath)))))

	r.HandleFunc("/index.html", indexHandler).Methods("GET")
	r.HandleFunc("/service-worker.js", serviceWorkerHandler).Methods("GET")
	r.HandleFunc("/client_id", clientIdHandler).Methods("GET")
	r.HandleFunc("/access_token", accessTokenHandler).Methods("POST")

	r.Methods("GET").PathPrefix("/").Handler(gziphandler.GzipHandler(serveIndex(distPath)))

	http.Handle("/", r)

	log.Println("Listening on port " + port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}

func serveIndex(distPath string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, distPath+"/index.html")
	})
}

func indexHandler(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, distPath+"/index.html")
}

func serviceWorkerHandler(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, distPath+"/service-worker.js")
}

func clientIdHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, clientId())
}

func accessTokenHandler(w http.ResponseWriter, r *http.Request) {
	if err := r.ParseMultipartForm(1024); err != nil {
		log.Println("could not parse form")
	}
	if r.PostFormValue("code") == "" {
		log.Println("no code provided")
		return
	}
	code := r.PostFormValue("code")

	reqBody, err := json.Marshal(map[string]string{
		"client_id":     clientId(),
		"client_secret": clientSecret(),
		"code":          code,
	})
	if err != nil {
		log.Println(err)
		return
	}

	req, err := http.NewRequest("POST",
		"https://github.com/login/oauth/access_token",
		bytes.NewBuffer(reqBody))
	if err != nil {
		log.Println(err)
		return
	}

	req.Header.Add("Content-Type", "application/json")
	req.Header.Add("Accept", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	defer resp.Body.Close()
	if err != nil {
		log.Println(err)
		return
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Println(err)
		return
	}

	fmt.Fprint(w, string(body))
}

// ENV VARIABLES

func environment() string {
	return os.Getenv("ENVIRONMENT")
}

func clientId() string {
	return os.Getenv("CLIENT_ID")
}

func clientSecret() string {
	return os.Getenv("CLIENT_SECRET")
}
