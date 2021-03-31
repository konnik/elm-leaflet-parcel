

import TileLayer from 'ol/layer/Tile';
import TileWMS from 'ol/source/TileWMS';

import { get as getProjection } from 'ol/proj';

export function skapa() {
    return new TileLayer({
        extent: getProjection('EPSG:3857').getExtent(),
        source: new TileWMS({
            url: 'https://karta.raa.se/lmhojdmodell',
            params: {
                "VERSION": "1.1.1",
                "FORMAT": "image/png",
                'LAYERS': 'terrangskuggning',
                'TILED': true
            },
        }),
    });
}
