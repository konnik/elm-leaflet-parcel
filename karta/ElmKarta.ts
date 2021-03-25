
import 'ol/ol.css';
import { skapa as skapaKarta, radera as raderaKarta, Karta } from "./Karta"

const kartor = {}
var sendToElm: Function;

export function registerApplication(app) {
    sendToElm = app.ports.kartaIncoming.send;
    app.kartaOutgoing.subscribe(recieveFromElm);
}


function recieveFromElm(message) {
    console.log(message);
    if (message.typ === "skapa_karta") {

        var elem = document.getElementById(message.id);
        console.log("element: ", elem)

        const karta = skapaKarta(message.id);
        karta.onEnkelklick(enkelklickHandler);
        kartor[message.id] = karta;
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
