import VectorLayer from 'ol/layer/Vector';
import VectorSource from 'ol/source/Vector';
import Feature from 'ol/Feature';
import Point from 'ol/geom/Point';
import Icon from 'ol/style/Icon';
import Style from 'ol/style/Style';
import IconAnchorUnits from 'ol/style/IconAnchorUnits';
import { fromLonLat } from 'ol/proj';
import { Coordinate } from 'ol/coordinate';
import { Options } from 'ol/layer/BaseVector';

export class MarkeringLayer extends VectorLayer {
    markeringar: Map<String, Feature>

    constructor() {


        super({
            source: new VectorSource({
                features: []
            }),
            visible: true,
            style: new Style({
                image: new Icon({
                    anchor: [0.5, 46],
                    anchorXUnits: IconAnchorUnits.FRACTION,
                    anchorYUnits: IconAnchorUnits.PIXELS,
                    src: 'https://openlayers.org/en/latest/examples/data/icon.png'
                })
            })
        });

        this.markeringar = new Map()
    }

    placeraMarkering(namn: string, lonLat: Coordinate): void {
        if (this.markeringar.has(namn)) {
            const m = this.markeringar.get(namn)
            m.setGeometry(new Point(fromLonLat(lonLat)));
        } else {
            const m = new Feature({
                geometry: new Point(fromLonLat(lonLat)),
                name: namn
            });
            this.markeringar.set(namn, m)
            this.getSource().addFeature(m)
        }
    }
}

export function skapa(): MarkeringLayer {
    return new MarkeringLayer();
}

