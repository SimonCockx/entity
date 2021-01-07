export interface JSONEvent {
    id: number;
    name: string;
    year: number;
    description: string;
}

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
        this._name = newName;
        this._isSynced = false;
    }

    private _year: number;
    public get year(): number {
        return this._year;
    }
    public set year(newYear: number) {
        this._year = newYear;
        this._isSynced = false;
    }

    private _description: string;
    public get description(): string {
        return this._description;
    }
    public set description(newDescription: string) {
        this._description = newDescription;
        this._isSynced = false;
    }

    public constructor(id: number, name: string, year: number, description: string) {
        this.id = id;
        this._name = name;
        this._year = year;
        this._description = description;

        this._isSynced = false;
    }

    public static fromJSON(jsonEvent: JSONEvent): Event {
        return new Event(jsonEvent.id, jsonEvent.name, jsonEvent.year, jsonEvent.description);
    }

    public toJSON(): JSONEvent {
        return {
            id: this.id,
            name: this.name,
            year: this.year,
            description: this.description,
        };
    }
}

export class InitialEvent {
    public readonly isSynced: false = false;
    public readonly id: null = null;

    public name: string;
    public year: number;
    public description: string;

    public constructor(name: string, year: number, description: string) {
        this.name = name;
        this.year = year;
        this.description = description;
    }

    public toJSON(): Omit<JSONEvent, 'id'> {
        return {
            name: this.name,
            year: this.year,
            description: this.description,
        };
    }
}
