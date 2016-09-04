import { Component, OnInit } from '@angular/core';
import { TestRandomService } from './test-random.service';


export class CellInfo {

  constructor(
    public text: string,
    public style: string
  ) {}
}

export function ci(text: string, style = ''): CellInfo
{
  return new CellInfo(text, style);
}

export function ci_ph(text: string): CellInfo
{
  return ci(text, 'prod_header');
}


export class TblData {
  constructor(
    public headers: CellInfo[],
    public cells: CellInfo[][]
  ) {}
}


@Component({
  moduleId: module.id,
  selector: 'app-table',
  templateUrl: 'table.component.html',
  styleUrls: ['table.component.css'],
  providers: [TestRandomService]
})
export class TableComponent implements OnInit {

  private _data: TblData = {
    headers: [ci('Waitting for data')],
    cells: [[ci('')]]
  };

  /* init example
  private _data: Data; = {
    headers: [
        ci_ph('PRODUCT'), ci('QTY BID'), ci('BID'), ci('ASK'), ci('QTY ASK')],
    cells: [
      [ ci_ph('PRD1'), ci('100'), ci('101.35'), ci('102.22'), ci('10') ],
      [ ci_ph('PRD2'), ci('100'), ci('101.35'), ci('102.22'), ci('10') ],
      [ ci_ph('PRD2'), ci('100'), ci('101.35'), ci('102.22'), ci('10') ]
    ]
  };*/


  constructor(private _testRandomService: TestRandomService) {
    this._testRandomService.eventTblUpdate.subscribe(
      tableData => this._data = tableData
    );
   };

  ngOnInit() {
  }

}
