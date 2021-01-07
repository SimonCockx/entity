export class Event {
  private _isSynced: boolean;
  public get isSynced(): boolean {
    return this._isSynced;
  }
  public readonly id: number;
  private _name: string;
  public get name(): string {
    return this._name;
  }
  public set name(newName: string) {
    this._isSynced = false;
    this._name = newName;
  }
  private _year: number;
  public get year(): number {
    return this._year;
  }
  public set year(newYear: number) {
    this._isSynced = false;
    this._year = newYear;
  }
  private _description: string;
  public get description(): string {
    return this._description;
  }
  public set description(newDescription: string) {
    this._isSynced = false;
    this._description = newDescription;
  }
  public constructor(
    id: number,
    name: string,
    year: number,
    description: string
  ) {
    this._name = name;
    this._year = year;
    this._description = description;
    this.id = id;
    this._isSynced = false;
  }
}
