import java.util._
import java.io._
import java.awt.Font
import java.awt.event.MouseEvent;
import scala.util.control.Breaks._

object tak{
  import java.io._
  
  private val pc=Array.tabulate(9)((x=>red("./pc/"+x)))
  private val user=Array.tabulate(9)((x=>red("./user/"+x)))
      new File("./pc").mkdirs()
    new File("./user").mkdirs()
    
  private def getset(get:Boolean,ar:String=null)(pcc:String,index:Int)=synchronized({
    if(get){
      if(pcc=="pc") pc(index) else user(index)
    }
    else{
      if(pcc=="pc") pc(index)=ar else user(index)=ar
    }
  })
    
  def get(pcc:String,index:Int)=getset(true)(pcc,index)
  
  def set(ar:String,pcc:String,index:Int)=getset(false,ar + get(pcc,index))(pcc,index)
  
  private def red(path:String):String={
    val file=new File(path)
    if(file.exists()==false)return ""
    val in=new Scanner(file)
    var s=""
    while(in.hasNextLine())s+=in.nextLine()+"\n"
    return s
  }
  
  
  override def finalize{
    println("in finalize")
    for(x<- 0 to 8){
    val l=new FileWriter("./pc/"+x)
    val l2=new FileWriter("./user/"+x)
    l.write(pc(x))
    l2.write(user(x))
    l.close()
    l2.close()
    }
  }
  
  def main(A:Array[String]){
    Runtime.runFinalizersOnExit(true)
    new tak().pcVSuser
  }
}

class tak {
  import tak._
  
    private var index=0
    private var data=Array(Array(' ',' ',' '),Array(' ',' ',' '),Array(' ',' ',' '))
    private val donot=new ArrayList[(Int,Int)]
    private val xturn,yturn=Array.fill(9)(-1)
    private val forced=Array ofDim[Boolean](9)
    private var pcstarted=false
    private var codi=(0,0)

  private var wonlos=""
  
  private val f=new javax.swing.JFrame()
  f.setAlwaysOnTop(true)
  private val p=new javax.swing.JPanel(){
    override def paintComponent(g2:java.awt.Graphics){
       val g=g2.asInstanceOf[java.awt.Graphics2D]
       g setRenderingHint(
       java.awt.RenderingHints KEY_ANTIALIASING,
       java.awt.RenderingHints VALUE_ANTIALIAS_ON
       )
       
       g setColor(java.awt.Color.GRAY)
       
       g.fillRect(0, 0, 300, 300)
       
      g setColor(java.awt.Color.BLACK)
      
      g drawLine(100,5,100,295)
      g drawLine(200,5,200,295)
      
      g drawLine(5,100,295,100)
      g drawLine(5,200,295,200)
    
      g setFont(new Font("Comic Sans MS",Font ITALIC,100))
      
      for(x<- 0 to 2;y<- 1 to 3)
        if(data(y-1)(x)=='X'){ g setColor(java.awt.Color.BLUE);g drawString("X",100*x+5,100*y-5)}
        else if(data(y-1)(x)=='O'){ g setColor(java.awt.Color.RED);g drawString("O",100*x+5,100*y-5)  }
      
       g setFont(new Font("Comic Sans MS",Font ITALIC,75))
       g setColor(java.awt.Color.RED)
       g drawString(wonlos,20,180)
      
    }
  }
  
  p.addMouseListener(new java.awt.event.MouseListener() {

			override def mouseClicked( arg0:MouseEvent) {
				// TODO Auto-generated method stub
				
			}

			override def mouseEntered( arg0:MouseEvent) {
				// TODO Auto-generated method stub
				
			}

			override def mouseExited( arg0:MouseEvent) {
				// TODO Auto-generated method stub
				
			}

			override 	def mousePressed( arg0:MouseEvent) {
				// TODO Auto-generated method stub
				
			}

			override def mouseReleased( e:MouseEvent) {
				// TODO Auto-generated method stub
			  println("released")
				val po=e.getPoint();
				codi=(po.y/100 toInt,po.x/100 toInt)
				println(codi _1,codi _2)
				if(data(codi _1)(codi _2)==' ')
				  t resume

			}
			
		});
  
  //f setUndecorated true
  f setSize(300,300)
  p.setSize(300,300)
  f add p
  f setVisible true
  f.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)
  
  private val t=new Thread(){
    override def run(){
      player1
    }
  }
    
  def pcVSpc{
    while(true){
      var me='X'
      pcstarted=true
      var con=check
      while(con._1==false){
      turn(logic(me),me)
      me=if(me=='X')'O' else 'X'
      pcstarted=if(pcstarted)false else true
      con=check
      Thread.sleep(150)
    }
      if(con._1&&(con._2=='X'||con._2=='O')){
        if(con._2=='O')pcstarted=true else pcstarted=false
        AI
      }
      reset
    }
    
  }
  
  def pcVSuser{
    t.start()
  }
  
  def player1{
    
    while(true){
      var con=(false,' ')
      if(false){turn(easy,'X');pcstarted=true}
      
    breakable(
    while(con._1 == false){
      Thread.sleep(500)
      println("pcstarted",pcstarted)
      turn(coordinate,'O')
      con=check
      Thread.sleep(500)
      if(con _1)break
      turn(logic(),'X')
      con=check
    }
    )
    println("status"+con)
    wonlos=if(con._2 == 'X')"LOSE" else if(con._2 == 'O')"WONN" else "DRAW"
    
    p.repaint();
    if(con._1 && con._2 =='O')AI
    Thread.sleep(700)
    reset
    p.repaint();
    }
  }
  
  def AI=this.synchronized({
    val l=scala.util.control.Breaks
    
    l.breakable({
      
      val pcindex=if(pcstarted)0 else 1
      
      for(x<- pcindex to 6 by 2  ){
        if(forced(x)==false&&forced(x+2)==true){
          println("AI WORKED")
          var st=x+" "
          val turn=xturn.zip(yturn)
          
          //val file=new java.io.FileWriter(if(pcstarted)"./pc" else "./user",true)
          
          for(r<-0 to x){
            st += xturn(r)+" "+yturn(r)+" "
          }
          
//          file.write(st+"\n")
//          file.close()
          
          if(pcstarted)
            set(st+"\n","pc",x)
            else 
            set(st+"\n","user",x)
          
          println("pc",get("pc",x))
          println("user",get("user",x))
          
          l.break()
        }
      }
      
      
    })
    
  }    
  )
  
  def turn(x:(Int,Int),me:Char){

    yturn(index)= x._2
    xturn(index)= x _1
    
    data(x _1)(x _2)=me
    tick
    index+=1
    donot.clear
  }
  
  
  def coordinate:(Int,Int)={
    while(true){
      t suspend;
      if(data(codi._1)(codi._2)==' ')return codi
    }
    return (0,0);
  }

  
  def logic(me:Char='X'):(Int,Int)={
    
    val l=checkForce(me)
    if(l._1){forced(index)=true;return l._2}
    
    for(k<- 0 to 1){
      //val in=new java.util.Scanner(new java.io.File(if(k==0)"./user" else "./pc"))
    val in=new java.util.Scanner(if(k==0)get("user",index) else get("pc",index))
      
    while(in.hasNext())
    if(in.nextInt()==index){
      
      breakable({
        for(x<- 0 until index)if(in.nextInt != xturn(x)||in.nextInt != yturn(x))break
        if(k==0 == pcstarted)return (in.nextInt,in.nextInt)
        else donot.add((in.nextInt,in.nextInt))
      })
      in nextLine
    }
    else in.nextLine()
    }
    
    easy
    
  }
  
  def checkForce(mee:Char):(Boolean,(Int,Int))={
    var me=mee
    
    for(d<- 0 to 1){
      for(x<- 0 to 2){
      val cont=data(x).count(_==me)
      if(data(x).count(_==' ')==1&&cont==2){        
        return (true,(x,data(x).indexOf(' ')))
      }
    }
    
    for(x<- 0 to 2){
      if(data(0)(x)==me&&data(1)(x)==me&&data(2)(x)==' ')return (true,(2,x))
      if(data(1)(x)==me&&data(2)(x)==me&&data(0)(x)==' ')return (true,(0,x))
      if(data(0)(x)==me&&data(2)(x)==me&&data(1)(x)==' ')return (true,(1,x))
    }
    
    if(data(1)(1)==me && data(2)(2)==me&&data(0)(0)==' ')return (true,(0,0))
    if(data(0)(0)==me && data(2)(2)==me&&data(1)(1)==' ')return (true,(1,1))
    if(data(1)(1)==me && data(0)(0)==me&&data(2)(2)==' ')return (true,(2,2))
    
    if(data(0)(2)==me && data(1)(1)==me&&data(2)(0)==' ')return (true,(2,0))
    if(data(1)(1)==me && data(2)(0)==me&&data(0)(2)==' ')return (true,(0,2))
    if(data(2)(0)==me && data(0)(2)==me&&data(1)(1)==' ')return (true,(1,1))
    me = if(me=='X')'O' else 'X'
    }
    
    (false ,(-1,-1))
  }
  
  def easy:(Int,Int)={
    var x,y=0
    var count=0
    while(true){
      x=((Math random)* 3 ) toInt;
      y=((Math random)* 3 ) toInt;
      if(count<50&&donot.contains((x,y))){println(s"prevented at $x , $y ");count+=1}
      else if(data(x)(y)==' ')return (x,y)
    }
    (x,y)
  }
  
  def check:(Boolean,Char)={
    var c='X'
    val l= scala.util.control.Breaks
    
   for(a<- 0 to 1){
     
     for(i<- 0 to 2)
      l breakable(
      for(j<- 0 to 2){
        if(data(i)(j) != c)l break;
        if(j==2)return (true,c)
      }    
      )
      
    for(i<- 0 to 2)
      l breakable(
      for(j<- 0 to 2){
        if(data(j)(i) != c)l break;
        else if(j==2)return (true,c)
      }    
      )
    
     l breakable(
     for(i<- 0 to 2)
      
      if(data(i)(i)!=c)l break
      else if(i==2)return (true,c)
      )
     
     var (a,b)=(0,2)
     l breakable({
     for(i<- 0 to 2){
      if(data(a)(b)!=c)l break
      else if(a==2 && b==0)return (true,c);
      a+=1
      b-=1
     }
        }
      )
     
     c='O'
 }
    
    for(z<- data;x<-z)
            if(x==' ')return (false , ' ')
    
    (true,' ')
  }
  
  def reset{
    for(x<-0 to 2;y<- 0 to 2)
      data(x)(y)=' '
      index=0
      wonlos=""
      pcstarted=false
      for(x<- 0 to 8){
        xturn(x) = -1
        yturn(x) = -1
        forced(x)=false
      }
        
  }
  
  def tick{
    p.repaint();
    println("\f")
    
    data foreach(
    x=>{
      x foreach(printf("%c\t",_))
      println
    }
    )
    println
  }
}
