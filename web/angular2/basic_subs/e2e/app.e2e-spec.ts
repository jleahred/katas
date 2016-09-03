import { BasicSubsPage } from './app.po';

describe('basic-subs App', function() {
  let page: BasicSubsPage;

  beforeEach(() => {
    page = new BasicSubsPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
