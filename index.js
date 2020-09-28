
import { Elm } from './src/Main.elm'

Elm.Main.init({
  node: document.querySelector('main')
})

import L from 'leaflet';
import {testeboan} from "./geo";

delete L.Icon.Default.prototype._getIconUrl;

L.Icon.Default.mergeOptions({
  iconRetinaUrl: require('leaflet/dist/images/marker-icon-2x.png'),
  iconUrl: require('leaflet/dist/images/marker-icon.png'),
  shadowUrl: require('leaflet/dist/images/marker-shadow.png'),
});


const gavle = [60.719459, 17.054273];
const sverige = [63.031926, 15.451756]

var map = L.map("map").setView(gavle, 10);

L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token={accessToken}', {
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18,
    id: 'mapbox/streets-v11',
    tileSize: 512,
    zoomOffset: -1,
    accessToken: 'pk.eyJ1Ijoia29ubmlrIiwiYSI6ImNrZm13azN2ajFmMWozMHFoZ3F3czFneW0ifQ.6vvhWXlFXhso6ip9QPMyqA'
}).addTo(map);

L.geoJSON(testeboan, {
    onEachFeature:  (feature, layer) => {
        console.log(feature.properties)
        if (feature.properties && feature.properties.name) {
            layer.bindPopup(feature.properties.name);
        }
    }
}).addTo(map);

