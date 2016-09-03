# BasicSubs

## Preparing

Small example on angular2

It will show how to subscribe to a service (push information)


To start scafolding...

```bash
ng new basic_subs
cd basic_subs
```

You can serve the proyect...

```bash
ng serve
```

Open the browser... `http://localhost:4200/`

Remember, the application will be reloaded on browser on any modification.

Lets create the testing component (called c1)...

```bash
ng g component c1
```

Integrate as a child component.

Edit `root component`  app.component.ts

Importing the component...

```typescript
import { C1Component } from './c1/c1.component';
```

Register...

```typescript
@Component({
  moduleId: module.id,
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.css'],
  directives: [C1Component]    // <<<<<<<<<<<<<<<<<<<<<<<
})
```

Add the component on root-component template editing `app.component.html`

```html
<app-c1>loading...</app-c1>
```

The output has to be...

```
app works!

c1 works!
```



## Adding the service on c1 component

```bash
cd src/app/c1
ng g service sc1
```

Now we have to inject dependency on c1 component editing c1.component.ts

```typescript
import { Sc1Service } from './sc1.service';

@Component({
  moduleId: module.id,
  selector: 'app-c1',
  templateUrl: 'c1.component.html',
  styleUrls: ['c1.component.css'],
  providers: [Sc1Service]
})
export class C1Component implements OnInit {

  constructor(private _sc1Service: Sc1Service) { }

```

Now, we will generate new values from service side. Edit c1.service to add `events`

```typescript
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs/Subject';


@Injectable()
export class Sc1Service {

  //  events
  private _eventUpdateString = new Subject<string>();
  public eventUpdateString = this._eventUpdateString.asObservable();

  //    timer
  private timerUpdateString() {
    this._eventUpdateString.next((Math.random()).toString());
    setTimeout(() => this.timerUpdateString(), 1000);
  };


  constructor() {
    this.timerUpdateString();
  }


}
```

Lets connect the service event on component model.

```typescript
export class C1Component implements OnInit {

  private randomString: string;

  constructor(private _sc1Service: Sc1Service) { 
    this._sc1Service.eventUpdateString.subscribe(
        randString =>  this.randomString = randString
    );
  }
```

Add randomString to template `c1.component.html`

```html
<p>
  c1 works!
  {{randomString}}
</p>
```


Done!!!




## More (pending)...


Run `ng build` to build the project. The build artifacts will be stored in the `dist/` directory. Use the `-prod` flag for a production build.

Run `ng test` to execute the unit tests via [Karma](https://karma-runner.github.io).

Run `ng e2e` to execute the end-to-end tests via [Protractor](http://www.protractortest.org/). 
Before running the tests make sure you are serving the app via `ng serve`.


Run `ng github-pages:deploy` to deploy to Github Pages.

## Further help

To get more help on the `angular-cli` use `ng --help` or go check out the [Angular-CLI README](https://github.com/angular/angular-cli/blob/master/README.md).
