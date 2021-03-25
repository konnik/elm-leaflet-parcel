
import { Elm } from './src/Lab.elm'
import 'ol/ol.css';
import { skapa as skapaKarta, radera as raderaKarta, Karta } from "./karta/Karta"

var app = Elm.Lab.init({
    node: document.getElementById('elm')
});


const sverigeExtent = [1118248.7982969885, 7696979.602230988, 2324119.356523609, 8764652.013318047]
const gavle = [60.72975, 17.12693];
const sverige = [63.031926, 15.451756]

function enkelklickHandler(karta, longLat) {
    karta.placeraMarkering(longLat);
    app.ports.kartaIncoming.send({
        "typ": "klick_i_karta",
        "id": karta.id,
        "lat": longLat[1],
        "long": longLat[0]
    });
};


const kartor = {}

app.ports.kartaOutgoing.subscribe(function (message) {
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

});


