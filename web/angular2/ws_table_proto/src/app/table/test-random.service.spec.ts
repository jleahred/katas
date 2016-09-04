/* tslint:disable:no-unused-variable */

import { addProviders, async, inject } from '@angular/core/testing';
import { TestRandomService } from './test-random.service';

describe('Service: TestRandom', () => {
  beforeEach(() => {
    addProviders([TestRandomService]);
  });

  it('should ...',
    inject([TestRandomService],
      (service: TestRandomService) => {
        expect(service).toBeTruthy();
      }));
});
