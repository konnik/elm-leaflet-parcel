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
    iconFeature: Feature

    constructor() {

        const iconFeature = new Feature({
            geometry: new Point(fromLonLat([17.198944862118232, 60.62266736314186])),
            name: 'Gävle någonstans...'
        });

        super({
            source: new VectorSource({
                features: [iconFeature]
            }),
            visible: false,
            style: new Style({
                image: new Icon({
                    anchor: [0.5, 46],
                    anchorXUnits: IconAnchorUnits.FRACTION,
                    anchorYUnits: IconAnchorUnits.PIXELS,
                    src: 'https://openlayers.org/en/latest/examples/data/icon.png'
                })
            })
        });

        this.iconFeature = iconFeature;
    }

    placeraMarkering(lonLat: Coordinate): void {
        this.iconFeature.setGeometry(new Point(fromLonLat(lonLat)));
        this.setVisible(true);
    }
}

export function skapa(): MarkeringLayer {
    return new MarkeringLayer();
}

