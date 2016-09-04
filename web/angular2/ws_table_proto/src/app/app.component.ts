import { Component } from '@angular/core';
import { TableComponent } from './table/table.component';

@Component({
  moduleId: module.id,
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.css'],
  directives: [TableComponent]
})
export class AppComponent {
  title = 'Table prototipe angular2';


}
