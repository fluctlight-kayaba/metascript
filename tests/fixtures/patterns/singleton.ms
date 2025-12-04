class Database {
    connection: string;
    static instance: Database | null;

    constructor() {
        this.connection = "Connected";
    }

    static getInstance(): Database {
        if (Database.instance === null) {
            Database.instance = new Database();
        }
        return Database.instance;
    }

    query(sql: string): void {
        console.log("Executing: " + sql);
    }
}
