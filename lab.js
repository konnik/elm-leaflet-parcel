
import { Elm } from './src/Lab.elm'

var app = Elm.Lab.init({
    node: document.getElementById('elm')
});


import 'ol/ol.css';
import { Map, View } from 'ol';
import TileLayer from 'ol/layer/Tile';
import OSM from 'ol/source/OSM';
import WMTS from 'ol/source/WMTS';
import WMTSTileGrid from 'ol/tilegrid/WMTS';
import TileWMS from 'ol/source/TileWMS';
import { get as getProjection } from 'ol/proj';
import { getTopLeft, getWidth, getCenter } from 'ol/extent';

console.log("map", document.getElementById("map").innerHTML)


/* const topowebbUrl = "https://api.lantmateriet.se/open/topowebb-ccby/v1/wmts/token/{accessToken}/1.0.0/{layer}/default/3857/{z}/{y}/{x}.png"
const topowebb = L.tileLayer(topowebbUrl, {
    accessToken: "b8913fdb-6555-3591-9cbb-6f1456ee1490",
    layer: "topowebb_nedtonad",
maxZoom: 15,
minZoom: 0,
continuousWorld: true,
attribution: '&copy; <a href="https://www.lantmateriet.se/en/">Lantmäteriet</a> Topografisk Webbkarta Visning, CCB',
});
 */

var projection = getProjection('EPSG:3857');
var projectionExtent = projection.getExtent();
console.log(projectionExtent)


const sverigeExtent = [1118248.7982969885, 7696979.602230988, 2324119.356523609, 8764652.013318047]

// XMin: 1118248.7982969885
// YMin: 7696979.602230988
// XMax: 2324119.356523609
// YMax: 8764652.013318047

var size = getWidth(projectionExtent) / 256;
var resolutions = new Array(14);
var matrixIds = new Array(14);
for (var z = 0; z < 14; ++z) {
    // generate resolutions and matrixIds arrays for this WMTS
    resolutions[z] = size / Math.pow(2, z);
    matrixIds[z] = z;
}

const gavle = [60.72975, 17.12693];
const sverige = [63.031926, 15.451756]

const topowebbLayer = new TileLayer({
    source: new WMTS({
        attributions: '&copy; <a href="https://www.lantmateriet.se/en/">Lantmäteriet</a> Topografisk Webbkarta Visning, CCB',
        url: "https://api.lantmateriet.se/open/topowebb-ccby/v1/wmts/token/b8913fdb-6555-3591-9cbb-6f1456ee1490/",
        verison: "1.0.0",
        layer: 'topowebb',
        style: 'default',
        wrapX: true,
        format: 'image/png',
        matrixSet: '3857',
        crossOrigin: 'anonymous',
        tileGrid: new WMTSTileGrid({
            tileSize: 256,
            origin: getTopLeft(projectionExtent),
            resolutions: resolutions,
            matrixIds: matrixIds,
        }),
    }),
})

// const orto = L.tileLayer.wms("https://stompunkt.lantmateriet.se/maps/ortofoto/wms/v1.3", {
//     layers: "Ortofoto_0.16,Ortofoto_0.25,Ortofoto_0.4,Ortofoto_0.5",
//     // layers: "orto025,orto050",
//     format: 'image/jpeg',
//     version: '1.1.1',

//     detectRetina: true
// });


const ortoLayer = new TileLayer({
    extent: projectionExtent,
    source: new TileWMS({
        url: 'https://stompunkt.lantmateriet.se/maps/ortofoto/wms/v1.3',
        params: { "VERSION": "1.1.1", "FORMAT": "image/jpeg", 'LAYERS': 'Ortofoto_0.16,Ortofoto_0.25,Ortofoto_0.4,Ortofoto_0.5', 'TILED': true },
    }),
})

console.log("center:", getCenter(sverigeExtent))

const map = new Map({
    target: 'map',
    layers: [ortoLayer],
    view: new View({
        center: [1909474.963338217, 8552634.820602188],
        zoom: 11
    })
});

window.korv = map;



