import { Elm } from './src/Main.elm';


if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}


if ("serviceWorker" in navigator) {
  navigator.serviceWorker.register("/service-worker.js")
    .then(function(registration) {
      console.log("Successfully registered service worker.");

      if (registration.active) {
        console.log("Service worker already active.");

        initElm();
      } else {
        console.log("Service worker not active, yet.");

        registration.onupdatefound = () => {
          const newWorker = registration.installing;

          console.log("A new service worker is being installed");
          console.log(newWorker);

          newWorker.addEventListener("statechange", () => {
            console.log("State changed.");

            if (newWorker.state === "activated") {
              console.log("Service worker is active now.")

              initElm();
            }
          });
        };
      }
    })
    .catch(function(error) {
      console.log("Registration failed with " + error);
    });
}


const initElm = () => {
  console.log("Initializing Elm application.");

  var app = Elm.Main.init({
    flags: {}
  });
};
