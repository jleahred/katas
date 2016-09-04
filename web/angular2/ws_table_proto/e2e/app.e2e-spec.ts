import { WsTableProtoPage } from './app.po';

describe('ws-table-proto App', function() {
  let page: WsTableProtoPage;

  beforeEach(() => {
    page = new WsTableProtoPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
