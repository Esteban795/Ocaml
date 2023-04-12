----------------
-- Exercice 2 --
----------------

-- Question 1

select count(*)
from books
where original_publication_year >= 2000;

-- Question 2

select sum(ratings_count) from books;

select count(*) from ratings ;

-- Question 3

select title, average_rating
from books
order by average_rating desc
limit 10;

-- Question 4

select count(distinct authors) 
from books;

-- Question 5

select title, authors
from books
where title like "%girl%";

----------------
-- Exercice 3 --
----------------

-- Quesiton 1

select user_id, avg(rating)
from ratings
group by user_id
order by avg(rating);

-- Question 2

select authors, sum(ratings_5)
from books
group by authors
order by sum(ratings_5) desc;

-- Question 3

select original_publication_year, count(*)
from books
group by original_publication_year;

-- Question 4

select avg(rating)
from books 
join ratings on books.book_id = ratings.book_id
where title = "1984";

-- Question 5

select title, authors, avg(rating)
from ratings join books on books.book_id = ratings.book_id
group by books.book_id
order by avg(rating) desc
limit 10;

-- Question 6

select authors, count(*)
from books
group by authors
having count(*) >= 3;

-- Qestion 7

select count(*)
from books join ratings on books.book_id = ratings.book_id
where original_publication_year = 2013;

----------------
-- Exercice 4 --
----------------

-- Question 1

select title, book_tags.count from books
join book_tags on books.book_id = book_tags.book_id
join tags on book_tags.tag_id = tags.tag_id
where tag_name = "classics" and book_tags.count > 500;

-- Question 2

select title, tag_name
from books 
join book_tags as bt on books.book_id = bt.book_id
join tags on tags.tag_id = bt.tag_id
join 
	(select book_id as id, max(count) as maxcount 
	from book_tags 
	group by book_id) 
on id = bt.book_id and count = maxcount;

-- Question 3

select book_id from books
except
select book_id from to_read;

-- Queston 4

select book_id, title from books
where book_id not in (select book_id from to_read);

-- Question 5

select r2.book_id, avg(r2.rating) - books.average_rating, books.title
from ratings as r1
join ratings as r2 on r1.user_id = r2.user_id
join books on r2.book_id = books.book_id
where r1.rating = 5 and r1.book_id = 1
group by r2.book_id
having count(*) > 10
order by avg(r2.rating) - books.average_rating desc;