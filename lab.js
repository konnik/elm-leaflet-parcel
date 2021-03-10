
import { Elm } from './src/Lab.elm'

var app = Elm.Lab.init({
    node: document.getElementById('elm')
});

app.ports.send.subscribe(function (message) {
    console.log(message);
});


import 'ol/ol.css';
import { skapa as skapaKarta, Karta } from "./karta/Karta"

const sverigeExtent = [1118248.7982969885, 7696979.602230988, 2324119.356523609, 8764652.013318047]
const gavle = [60.72975, 17.12693];
const sverige = [63.031926, 15.451756]


const karta1 = skapaKarta("karta1");
const karta2 = skapaKarta("karta2");

karta1.onEnkelklick(function (karta, longLat) {
    karta.placeraMarkering(longLat);
    app.ports.receive.send({ "typ": "click", "lat": longLat[1], "long": longLat[0] });
});

karta2.onEnkelklick(function (karta, longLat) {
    karta.placeraMarkering(longLat);
    app.ports.receive.send({ "typ": "click", "lat": longLat[1], "long": longLat[0] });
});


app.ports.send.subscribe(function (message) {
    if (message.typ === "visa_lager") {
        karta1.valjBakgrundslager(message.namn);
        karta2.valjBakgrundslager(message.namn);
    }
});


