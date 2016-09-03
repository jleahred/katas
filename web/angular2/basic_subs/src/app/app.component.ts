import { Component } from '@angular/core';
import { C1Component } from './c1/c1.component';

@Component({
  moduleId: module.id,
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.css'],
  directives: [C1Component]
})
export class AppComponent {
  title = 'app works!';
}
