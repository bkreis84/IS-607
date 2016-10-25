-- #1
select count(speed) from planes where speed is not null;
-- 23
select max(speed) from planes;
-- 432
select min(speed) from planes;
-- 90

-- #2
select sum(distance) from flights 
where distance is not null and month=1 and year=2013;
-- 27,188,805
select sum(distance) from flights
where tailnum is null and month=1 and year=2013;
-- 81,763

-- #3
select planes.manufacturer, sum(flights.distance)
from flights join planes
on planes.tailnum=flights.tailnum
where flights.year=2013 and flights.month=7 and flights.day=5
group by planes.manufacturer order by manufacturer;

select planes.manufacturer, sum(flights.distance)
from flights left join planes
on planes.tailnum=flights.tailnum
where flights.year=2013 and flights.month=7 and flights.day=5
group by planes.manufacturer order by manufacturer;
-- Inner join only includes matches and ignores null values, unlike the outer join which 
-- showed an additional 127,671 miles in flights where the manufacturer was not available


-- Which airlines had flights in 2013 (and how many flights) from aircraft manufactured 
-- on or before the year 1990?
select airlines.name, count(flights.flight)
from airlines join planes left join flights
on planes.tailnum=flights.tailnum
and airlines.carrier=flights.carrier
where planes.year<=1990
and flights.year=2013
group by airlines.name order by airlines.name;

