
import 'ol/ol.css';
import { skapa as skapaKarta, radera as raderaKarta, Karta } from "./Karta"

const kartor = {}
var sendToElm = (msg) => { }

export function registerApplication(app) {
    console.log("Registrerar Elm-app med portar:", app.ports);

    if (app.ports.kartaOutgoing) {
        app.ports.kartaOutgoing.subscribe(recieveFromElm);
    } else {
        console.log("Varning: port 'kartaOutgoing' saknas. Inga händelser kommer att tas emot från Elm-appen.")
    }

    if (app.ports.kartaIncoming) {
        sendToElm = app.ports.kartaIncoming.send;
    } else {
        console.log("Varning: port 'kartaIncoming' saknas. Inga händelser kommer att skickas till Elm-appen.")
    }
}


function recieveFromElm(message) {
    console.log(message);
    if (message.typ === "skapa_karta") {
        setTimeout(() => {

            var elem = document.getElementById(message.id);
            console.log("element: ", elem)

            const karta = skapaKarta(message.id);
            karta.onEnkelklick(enkelklickHandler);
            kartor[message.id] = karta;

            if (message.markering) {
                karta.placeraMarkering([message.markering.long, message.markering.lat]);
                karta.centreraKarta([message.markering.long, message.markering.lat]);
            }

        }, 100)
    }

    if (message.typ === "radera_karta") {
        raderaKarta(kartor[message.id]);
        delete kartor[message.id];
    }

    if (message.typ === "visa_lager") {
        kartor[message.id].valjBakgrundslager(message.lagernamn);
    }

}

function enkelklickHandler(karta, longLat) {
    karta.placeraMarkering(longLat);
    sendToElm({
        "typ": "klick_i_karta",
        "id": karta.id,
        "lat": longLat[1],
        "long": longLat[0]
    });
};
