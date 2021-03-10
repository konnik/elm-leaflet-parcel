

import TileLayer from 'ol/layer/Tile';
import TileWMS from 'ol/source/TileWMS';

import { get as getProjection } from 'ol/proj';

export function skapa() {
    return new TileLayer({
        extent: getProjection('EPSG:3857').getExtent(),
        source: new TileWMS({
            url: 'https://stompunkt.lantmateriet.se/maps/ortofoto/wms/v1.3',
            params: { "VERSION": "1.1.1", "FORMAT": "image/jpeg", 'LAYERS': 'Ortofoto_0.16,Ortofoto_0.25,Ortofoto_0.4,Ortofoto_0.5', 'TILED': true },
        }),
    });
}
