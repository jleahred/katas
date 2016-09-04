import { Injectable } from '@angular/core';
import { Subject } from 'rxjs/Subject';
import { TblData, ci, ci_ph } from './table.component';


function getRndQty(): string {
  return (Math.random() * 100 + 1).toFixed(0);
}

function getRndPrice(): string {
  return (Math.random() * 100 + 70).toFixed(3);
}

function getRandProd(prdName: string) {
  return [ ci_ph(prdName), ci(getRndQty()), ci(getRndPrice()), ci(getRndPrice()), ci(getRndQty()) ]; 
}

function getRandomTblData(): TblData {
  return  {
    headers: [
        ci_ph('PRODUCT'), ci('QTY BID'), ci('BID'), ci('ASK'), ci('QTY ASK')],
    cells: [
      getRandProd('PRODUCT1'),
      getRandProd('PRODUCT2'),
      getRandProd('PRODUCT3'),
      getRandProd('PRODUCT4'),
      getRandProd('PRODUCT5'),
      getRandProd('PRODUCT6'),
      getRandProd('PRODUCT7'),
      getRandProd('PRODUCT8'),
      getRandProd('PRODUCT9'),
      getRandProd('PRODUCT10'),
      getRandProd('PRODUCT11'),
    ]
  };
}


@Injectable()
export class TestRandomService {

  //  events
  private _eventTblUpdate = new Subject<TblData>();
  public eventTblUpdate = this._eventTblUpdate.asObservable();


  //  timer
  private timerGenData() {
    this._eventTblUpdate.next(getRandomTblData());
    setTimeout(() => this.timerGenData(), 500);
  };


  constructor() {
    this.timerGenData();
  }

}



