import { Injectable } from '@angular/core';
import { Subject } from 'rxjs/Subject';


@Injectable()
export class Sc1Service {

  //  events
  private _eventUpdateString = new Subject<string>();
  public eventUpdateString = this._eventUpdateString.asObservable();

  //  timer
  private timerUpdateString() {
    this._eventUpdateString.next((Math.random()).toString());
    setTimeout(() => this.timerUpdateString(), 1000);
  };


  constructor() {
    this.timerUpdateString();
  }

}
