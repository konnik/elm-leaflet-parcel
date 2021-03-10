import TileLayer from "ol/layer/Tile";
import WMTS from "ol/source/WMTS";
import WMTSTileGrid from "ol/tilegrid/WMTS";
import { get as getProjection, toLonLat, fromLonLat } from 'ol/proj';
import { getTopLeft, getWidth, getCenter } from 'ol/extent';


export enum TopowebbVariant {
    NORMAL = "topowebb",
    NEDTONAD = "topowebb_nedtonad"
}

var projection = getProjection('EPSG:3857');
var projectionExtent = projection.getExtent();

var size = getWidth(projectionExtent) / 256;
var resolutions = new Array(14);
var matrixIds = new Array(14);
for (var z = 0; z < 14; ++z) {
    // generate resolutions and matrixIds arrays for this WMTS
    resolutions[z] = size / Math.pow(2, z);
    matrixIds[z] = z;
}

export function skapa(variant: TopowebbVariant = TopowebbVariant.NORMAL): TileLayer {
    return new TileLayer({
        source: new WMTS({
            attributions: '&copy; <a href="https://www.lantmateriet.se/en/">Lantmäteriet</a> Topografisk Webbkarta Visning, CCB',
            url: "https://api.lantmateriet.se/open/topowebb-ccby/v1/wmts/token/b8913fdb-6555-3591-9cbb-6f1456ee1490/",
            version: "1.0.0",
            layer: variant,
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
    });
}

