# Scala-Project-2021



This project was created for a Master degree's course. The creators of this project were me, Rute Santos and Filipe Ferreira. 

The main objective of this work is the design and development of an application using functional programming techniques.

The problem to solve involves the scheduling of production orders in a factory. Each production order defines a number of products to be produced. The production process of each product is a linear sequence of several tasks. Each task is performed using a subset of the physical resources available and allocates them for its duration. For each physical resource, a human resource must be allocated to handle it. The human resources have skills and can only handle a subset of the physical resources.

The model diagram is as follows:

![DomainModel](https://user-images.githubusercontent.com/46728174/131755625-05572b79-d97e-436e-b9a5-f77dcf107625.png)



## Milestone 1 ##

In this milestone, some constraints of the complete problem will be relaxed. The generation of the production schedule in this milestone assumes:

1. The products are produced in the order stated by the input file; 

2) Only one task at a time is processed in the facility;
3) The processing task, as it is the only one, has access to all the physical and human resources.

## Milestone 2  

The properties to consider are, for instance: 

1. The same resource cannot be used at the same time by two tasks. 
2.  The complete schedule must schedule all the tasks of all the products needed.

In this milestone Property based tests were also used.

## Milestone 3 

This milestone tries to minimize the complete production time by allowing both more than one task to be performed at the same time, and the alteration of the order of product production. The production of one product must still be linear, but more than one product, from any order, can be in production at the same time. In this milestone, the generation of the production schedule assumes: 

1) The products are produced in an optimizing order; 
2) More than one task can be performed at a time; 
3) Tasks can only be performed in parallel with others if there are enough physical & human resources.
