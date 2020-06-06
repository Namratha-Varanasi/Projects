import { Component } from '@angular/core';

import { Todo } from "./todo";                             // same directory ./ another directory in same parent ../
import { ValueConverter } from '@angular/compiler/src/render3/view/template';
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'Todo-app';
  todoValue:string;
  list:Todo[];

  ngOnInit(){
    this.list=[];
    this.todoValue="";
  }

  addItem(){
    if(this.todoValue!==""){
      const newItem:Todo ={
        id:Date.now(),
        value:this.todoValue,
        isDone:false
      };
      this.list.push(newItem);
    }
    this.todoValue=""; //to clear out the input box after adding
  }

  deleteItem(id:number){
    this.list=this.list.filter(item=>item.id!==id); //filter and display all the items where item whose id is not equal to given id

  }
 
  


/*constructor(){
  console.log("constuctor called");
  this.doSomething('Namratha');
}

doSomething(val:string):void{
val="Awesome";

}*/


}
