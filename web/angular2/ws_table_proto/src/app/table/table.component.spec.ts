/* tslint:disable:no-unused-variable */

import { By }           from '@angular/platform-browser';
import { DebugElement } from '@angular/core';
import { addProviders, async, inject } from '@angular/core/testing';
import { TableComponent } from './table.component';
import { TestRandomService } from './test-random.service';

describe('Component: Table', () => {
  it('should create an instance', () => {
    let component = new TableComponent(new TestRandomService());
    expect(component).toBeTruthy();
  });
});
