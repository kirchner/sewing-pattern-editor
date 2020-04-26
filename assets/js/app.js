// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import "../css/app.css";
import '@fortawesome/fontawesome-free/js/fontawesome';
import '@fortawesome/fontawesome-free/js/solid';
import '@fortawesome/fontawesome-free/js/brands';

import './jspdf.min.js';
import './svg2pdf.js';

import './cv.js';
import './aruco.js';
import './svd.js';
import './posit1.js';

// webpack automatically bundles all modules in your
// entry points. Those entry points can be configured
// in "webpack.config.js".
//
// Import dependencies
//
import "phoenix_html";

// Import local files
//
// Local files can be imported directly using relative paths, for example:
// import socket from "./socket"

import { Elm } from "../src/Main.elm";


const initElm = () => {
  var app = Elm.Main.init({
    flags: {
      csrfToken: csrfToken(),
    }
  });

  if (app.ports.selectAllTextIn) {
    app.ports.selectAllTextIn.subscribe(id => {
      window.requestAnimationFrame(() => {
        const input = document.getElementById(id);

        if (input !== null) {
          input.select();
        }
      });
    });
  }

  if (app.ports.storeCache) {
    app.ports.storeCache.subscribe(function(data) {
      localStorage.setItem(data.key, data.value);

      setTimeout(function() {
        app.ports.onStoreChange.send({ key: data.key, value: data.value });
      }, 0);
    });
  }

  if (app.ports.requestCache) {
    app.ports.requestCache.subscribe(function(data) {
      var value = localStorage.getItem(data.key);

      if (value !== null) {
        app.ports.onStoreChange.send({ key: data.key, value: value });
      } else {
        app.ports.onStoreMissing.send({ key: data.key });
      }
    });
  }

  window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage) {
      if (app.ports.onStoreChange) {
        app.ports.onStoreChange.send({ key: event.key, value: event.newValue });
      }
    }
  });

  var setViewport = function(canvas, width, height) {
    canvas.setAttribute("width", width);
    canvas.setAttribute("height", height);
  };

  if (app.ports.startVideo) {
    app.ports.startVideo.subscribe(function(data) {
      var canvasForDisplay = document.getElementById("for-display");
      var contextDisplay = canvasForDisplay.getContext("2d");

      var displayWidth = data.width;
      var displayHeight = data.height;

      setViewport(canvasForDisplay, displayWidth, displayHeight);

      app.ports.resizeVideo.subscribe(function(resizeData) {
        displayWidth = resizeData.width;
        displayHeight = resizeData.height;

        setViewport(canvasForDisplay, displayWidth, displayHeight);
      });

      var video = document.querySelector("video");

      if (video) {
        navigator.mediaDevices.getUserMedia({
          audio: false,
          video: {
            facingMode: "environment"
          }
        })
          .then(function(mediaStream) {
            video.srcObject = mediaStream;
            video.onloadedmetadata = function() {
              video.play();
            };
          })
          .catch(function(err) {
            console.log(err.name + ": " + err.message);
          });

        var canvasForComputation = document.getElementById("for-computation");
        var contextComputation = canvasForComputation.getContext("2d");

        detector = new AR.Detector();

        var tick = function() {
          requestAnimationFrame(tick);

          if (video.readyState === video.HAVE_ENOUGH_DATA) {
            var videoWidth = video.videoWidth;
            var videoHeight = video.videoHeight;

            if (videoHeight / videoWidth < displayHeight / displayWidth) {
              contextDisplay.drawImage(
                video,

                (videoWidth * displayHeight / videoHeight  -  displayWidth) / 2,
                0,
                displayWidth * videoHeight / displayHeight,
                videoHeight,

                0, 0, displayWidth, displayHeight
              );
            } else {
              contextDisplay.drawImage(
                video,

                0,
                (videoHeight * displayWidth / videoWidth  -  displayHeight) / 2,
                videoWidth,
                displayHeight * videoWidth / displayWidth,

                0, 0, displayWidth, displayHeight
              );
            }

            app.ports.changedCamera.send({
              camera: {
                width: videoWidth,
                height: videoHeight
              }
            });

            canvasForComputation.setAttribute("width", videoWidth);
            canvasForComputation.setAttribute("height", videoHeight);

            contextComputation.drawImage(
              video,
              0, 0, videoWidth, videoHeight,
              0, 0, videoWidth, videoHeight
            );

            var imageData = contextComputation.getImageData(0, 0, videoWidth, videoHeight);
            var markers = detector.detect(imageData);

            if (markers.length > 0){
              var corners = markers[0].corners;

              for (i = 0; i < corners.length; ++ i){
                corner = corners[i];

                corner.x = corner.x - (videoWidth / 2);
                corner.y = (videoHeight / 2) - corner.y;
              }

              var posit = new POS.Posit(81.0, videoWidth);

              var pose = posit.pose(corners);
              var rotation = pose.bestRotation;
              var translation = pose.bestTranslation;

              app.ports.changedPose.send({
                pose: {
                  rotation: rotation[0].concat(rotation[1], rotation[2]),
                  translation: {
                    x: translation[0],
                    y: translation[1],
                    z: translation[2]
                  }
                }
              });
            }
          }
        };
        requestAnimationFrame(tick);
      }
    });
  }

  app.ports.printDetail.subscribe(function(ids) {
    var pdf = new jsPDF({
      orientation: 'p',
      unit: 'mm',
      format: 'a4',
    });

    ids.forEach(function(id, index) {
      var svg = document.getElementById(id);

      svg2pdf(svg, pdf, {
        xOffset: 0,
        yOffset: 0,
        scale: 1,
      });

      if (index < ids.length - 1) {
        pdf.addPage();
      }
    });

    pdf.save('detail.pdf');
  });

  return app;
};

window.addEventListener("DOMContentLoaded", function() {
  initElm();
});


function csrfToken() {
  var csrfTokenElements = document.getElementsByName('csrf-token');

  if (csrfTokenElements && csrfTokenElements[0]) {
    return csrfTokenElements[0].content;
  } else {
    return "";
  }
}
