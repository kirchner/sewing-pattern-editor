package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/exec"

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

	if environment() == "production" {
		r.Methods("GET").PathPrefix("/static").
			Handler(http.StripPrefix("/static",
				gziphandler.GzipHandler(http.FileServer(http.Dir(distPath)))))
	} else if environment() == "development" {
		go execParcel()

		r.Methods("GET").PathPrefix("/static").
			Handler(http.StripPrefix("/static",
				gziphandler.GzipHandler(serveAsset())))
	}

	r.HandleFunc("/client_id", clientIdHandler).Methods("GET")
	r.HandleFunc("/access_token", accessTokenHandler).Methods("POST")

	r.Methods("GET").PathPrefix("/").Handler(gziphandler.GzipHandler(serveIndex(distPath)))

	http.Handle("/", r)

	log.Println("Listening on port " + port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}

func serveAsset() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp, err := http.Get("http://localhost:1234/static" + r.URL.Path)
		defer resp.Body.Close()
		if err != nil {
			log.Println(err)
		}

		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Println(err)
			return
		}

		w.Header().Add("Content-Type", resp.Header.Get("Content-Type"))

		fmt.Fprint(w, string(body))
	})
}

func serveIndex(distPath string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if environment() == "production" {
			http.ServeFile(w, r, distPath+"/index.html")
		} else if environment() == "development" {
			resp, err := http.Get("http://localhost:1234/index.html")
			defer resp.Body.Close()
			if err != nil {
				log.Println(err)
			}

			body, err := ioutil.ReadAll(resp.Body)
			if err != nil {
				log.Println(err)
				return
			}

			w.Header().Add("Content-Type", resp.Header.Get("Content-Type"))

			fmt.Fprint(w, string(body))
		}
	})
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

func execParcel() {
	cmd := exec.Command("yarn", "parcel", "serve", "index.html", "--public-url=/static", "--port=1234")
	cmd.Stdout = os.Stdout
	err := cmd.Start()
	if err != nil {
		log.Fatal(err)
	}
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
