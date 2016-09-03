/* tslint:disable:no-unused-variable */

import { addProviders, async, inject } from '@angular/core/testing';
import { Sc1Service } from './sc1.service';

describe('Service: Sc1', () => {
  beforeEach(() => {
    addProviders([Sc1Service]);
  });

  it('should ...',
    inject([Sc1Service],
      (service: Sc1Service) => {
        expect(service).toBeTruthy();
      }));
});
