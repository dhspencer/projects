You have detalls of some companies that trade internationally and strengthen the economy of their countries. You know the nationality of each company and you have a list of trades between the various companies. Your task is to generate a summary that consists of sums of the value of goods imported and exported by every country.
Note that when a company buys some goods, it contributes to its country's total import, and when the company sells some goods, it contributes to its country's total export.

You are given two tables: companies and trades, with the
following structure:

create table companies (
name varchar (30) not null,
country varchar (30) not null,
unique (name)
)

create table trades (
id integer not null,
seller varchar(30) not null,
buyer varchar (30) not null,
value int not null
)

A row in table companies contains the name of a company and the name of the nationality of the company

A row in table trades contains the unique ID of the trade, the name of the selling company, the name of the buying company and the value of the traded goods.

Write an SQL query that returns a table consisting of three columns, country, export, import, which contain the sums f the values of the exported (sold to other countries) and imported (purchased from other countries) goods for every country.
Each country should appear in this table. The result table should be sorted increasingly by country.

For example, for:
companies:

name			country
Alice s.p.		Wonderland
Y-zap			Wonderland
Absolute		Mathlands
Arcus			Mathlands
Lil Mermaid		Underwater Kingdom
None at all		Nothingland

trades:

id			seller			buyer			value
20121107	Li1 Mermaid 	Alice s.p.		10
20123112	Arcus t.g.		Y-zap			30
20120125	Alice S.P.		Arcus t.g.		100
20120216	Lil Mermaid		Absolute		30
20120217	Lil Mermaid		Absolute		50

Your query should return:

country					export		import
Mathlands				30				180
Nothingland				0				0
Underwater Kingdom		90				0
Wonderland				100				40

Assume that:
• There is no trade between companies within a single country;
• Every company in the table trades also appears in the table companies;
• Every company appears in table companies exactly once.