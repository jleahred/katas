import { Component, OnInit } from '@angular/core';
import { Sc1Service } from './sc1.service';

@Component({
  moduleId: module.id,
  selector: 'app-c1',
  templateUrl: 'c1.component.html',
  styleUrls: ['c1.component.css'],
  providers: [Sc1Service]
})
export class C1Component implements OnInit {

  private randomString: string;

  constructor(private _sc1Service: Sc1Service) { 
    this._sc1Service.eventUpdateString.subscribe(
        randString =>  this.randomString = randString
    );
  }

  ngOnInit() {
  }

}
