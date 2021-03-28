import 'ol/ol.css';
import { Map, View } from 'ol';
import { toLonLat, fromLonLat } from 'ol/proj';
import { skapa as skapaTopowebb, TopowebbVariant } from "./lager/Topowebb"
import { skapa as skapaOrtofoto } from "./lager/Ortofoto"
import { MarkeringLayer, skapa as skapaMarkering } from './lager/Markering';
import { Coordinate } from 'ol/coordinate';

export enum Bakgrundslager {
    ORTOFOTO = "ortofoto",
    TOPOWEBB = "topowebb_normal",
    TOPOWEBB_NEDTONAD = "topowebb_nedtonad"
}

export class Karta {
    id: string;
    map: Map;
    ortoLayer = skapaOrtofoto();
    topowebbLayer = skapaTopowebb(TopowebbVariant.NORMAL);
    topowebbnedtonadLayer = skapaTopowebb(TopowebbVariant.NEDTONAD);
    markeringLager: MarkeringLayer = skapaMarkering();

    constructor(id: string, bakgrundslager: Bakgrundslager = Bakgrundslager.TOPOWEBB) {
        this.id = id;
        this.ortoLayer = skapaOrtofoto();
        this.topowebbLayer = skapaTopowebb(TopowebbVariant.NORMAL);
        this.topowebbnedtonadLayer = skapaTopowebb(TopowebbVariant.NEDTONAD);
        this.markeringLager = skapaMarkering();

        this.map = new Map({
            target: id,
            layers: [this.ortoLayer, this.topowebbLayer, this.topowebbnedtonadLayer, this.markeringLager],
            view: new View({
                center: [1909474.963338217, 8552634.820602188],
                zoom: 11
            })
        });

        this.valjBakgrundslager(bakgrundslager);
    }

    valjBakgrundslager(lager: Bakgrundslager): void {
        this.ortoLayer.setVisible(lager === Bakgrundslager.ORTOFOTO);
        this.topowebbLayer.setVisible(lager === Bakgrundslager.TOPOWEBB);
        this.topowebbnedtonadLayer.setVisible(lager === Bakgrundslager.TOPOWEBB_NEDTONAD);
    }

    placeraMarkering(namn, longLat: Coordinate): void {
        this.markeringLager.placeraMarkering(namn, longLat);
        this.markeringLager.setVisible(true);
    }

    centreraKarta(longLat: Coordinate): void {
        this.map.getView().setCenter(fromLonLat(longLat))
    }

    onEnkelklick(callback: (a: Karta, b: Coordinate) => void) {
        const that = this;
        this.map.on('singleclick', function (event) {
            const lonLat = toLonLat(event.coordinate)
            callback(that, lonLat);
        });
    }
}

export function skapa(id: string): Karta {
    return new Karta(id);
}

export function radera(karta: Karta): void {
    karta.map.dispose();
}


