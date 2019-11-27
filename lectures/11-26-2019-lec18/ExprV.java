import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
abstract class Expr {
  <T> T visit(ExprVisitor<T> v) {
    throw new RuntimeException("Class did not implement visit method for ExprVisitor: " + this.getClass().toString());
  }
}
interface ExprVisitor<T> {
  T visit(ELet e);
  T visit(EId e);
  T visit(ENum e);
  T visit(EPlus e);
}
class ELet extends Expr {
  String name; Expr val, body;
  ELet(String name, Expr val, Expr body) { this.name = name; this.val = val; this.body = body; }
  public <T> T visit(ExprVisitor<T> v) { return v.visit(this); }
}
class EId extends Expr {
  String name;
  EId(String name) { this.name = name; }
  public <T> T visit(ExprVisitor<T> v) { return v.visit(this); }
}
class ENum extends Expr {
  int num;
  ENum(int num) { this.num = num; }
  public <T> T visit(ExprVisitor<T> v) { return v.visit(this); }
}
class EPlus extends Expr {
  Expr left, right;
  EPlus(Expr left, Expr right) { this.left = left; this.right = right; }
  public <T> T visit(ExprVisitor<T> v) { return v.visit(this); }
}

class UnboundIdsVisitor implements ExprVisitor<ArrayList<String>> {
  Map<String, Boolean> env;
  public UnboundIdsVisitor() {
    this.env = new HashMap<>();
  }
  public UnboundIdsVisitor(Map<String, Boolean> env) {
    this.env = env;
  }
  ExprVisitor<ArrayList<String>> constructMe(Map<String, Boolean> env) {
    return new UnboundIdsVisitor(env);
  }
  public ArrayList<String> visit(ELet e) {
    ArrayList<String> fromVal = e.val.visit(this);
    Map<String, Boolean> copied = new HashMap<>(this.env);
    copied.put(e.name, true);
    ExprVisitor<ArrayList<String>> newVisitor = this.constructMe(copied);
    System.out.println("Constructing a new visitor in ELet visit: " + newVisitor.getClass().toString());
    fromVal.addAll(e.body.visit(newVisitor));
    return fromVal;
  }
  public ArrayList<String> visit(EId e) {
    ArrayList<String> toReturn = new ArrayList<String>();
    if(!this.env.containsKey(e.name)) { toReturn.add(e.name); }
    return toReturn;
  }
  public ArrayList<String> visit(ENum e) {
    ArrayList<String> toReturn = new ArrayList<String>();
    return toReturn;
  }
  public ArrayList<String> visit(EPlus e) {
    ArrayList<String> left = e.left.visit(this);
    ArrayList<String> right = e.right.visit(this);
    left.addAll(right);
    return left;
  }
}

/* The code below is fairly gnarly, BUT it adds EMinus without touching any of
   the code above at all. A language design question is how to make this
   pattern into a language feature that does better type-checking -- note the
   casting and runtime errors in the top-level abstract classes and
   associated comments.

  */

interface ExprVisitorMinus<T> extends ExprVisitor<T> {
  T visit(EMinus e);
}
abstract class ExprMinus extends Expr {
  <T> T visit(ExprVisitor<T> v) {
    if(v instanceof ExprVisitorMinus) {
      return this.visit((ExprVisitorMinus<T>)v);
    }
    throw new RuntimeException("Needed a more specific visitor to handle expressions containing Minus: " + v.getClass().toString());
  }
  // This class should be implemented by all the classes that _directly_
  <T> T visit(ExprVisitorMinus<T> v) {
    throw new RuntimeException("Expr class did not implement visit method for ExprVisitorMinus: " + this.getClass().toString());
  }
}
class EMinus extends ExprMinus {
  Expr left, right;
  EMinus(Expr left, Expr right) { this.left = left; this.right = right; }
  public <T> T visit(ExprVisitorMinus<T> v) { return v.visit(this); }
}
class UnboundIdsVisitorMinus extends UnboundIdsVisitor implements ExprVisitorMinus<ArrayList<String>> {
  public UnboundIdsVisitorMinus(Map<String, Boolean> env) { super(env); }
  public UnboundIdsVisitorMinus() { super(); }
  public ArrayList<String> visit(EMinus e) {
    ArrayList<String> left = e.left.visit(this);
    ArrayList<String> right = e.right.visit(this);
    left.addAll(right);
    return left;
  }
  public ExprVisitor<ArrayList<String>> constructMe(Map<String, Boolean> env) {
    return new UnboundIdsVisitorMinus(env);
  }
}

/* 
 * Here's another conservative extension that adds a new expression, lambda,
 * that has an argument that affects the environment
 */
interface ExprVisitorMinusAndLam<T> extends ExprVisitorMinus<T> {
  T visit(ELam e);
}
abstract class ExprMinusAndLam extends ExprMinus {
  <T> T visit(ExprVisitor<T> v) {
    if(v instanceof ExprVisitorMinusAndLam) {
      return this.visit((ExprVisitorMinusAndLam<T>)v);
    }
    throw new RuntimeException("Class did not implement visit method for ExprVisitorMinusAndLam: " + this.getClass().toString());
  }
  <T> T visit(ExprVisitorMinusAndLam<T> v) {
    throw new RuntimeException("Class did not implement visit method for ExprVisitorMinusAndLam: " + this.getClass().toString());
  }
}
class ELam extends ExprMinusAndLam {
  String name;
  Expr body;
  ELam(String name, Expr body) { this.name = name; this.body = body; }
  public <T> T visit(ExprVisitorMinusAndLam<T> v) { return v.visit(this); }
}
class UnboundIdsVisitorMinusAndLam extends UnboundIdsVisitorMinus implements ExprVisitorMinusAndLam<ArrayList<String>> {
  public UnboundIdsVisitorMinusAndLam(Map<String, Boolean> env) { super(env); }
  public UnboundIdsVisitorMinusAndLam() { super(); }
  public ArrayList<String> visit(ELam e) {
    Map<String, Boolean> copied = new HashMap<String, Boolean>(this.env);
    copied.put(e.name, true);
    return e.body.visit(this.constructMe(copied));
  }
  public ExprVisitor<ArrayList<String>> constructMe(Map<String, Boolean> env) {
    return new UnboundIdsVisitorMinusAndLam(env);
  }
}



public class ExprV {
  public static void main(String[] args) {
    Expr test = new ELet("x", new EId("x"), new EPlus(new EId("y"), new EId("x")));
    System.out.println(test.visit(new UnboundIdsVisitor()));
    Expr test2 = new ELet("x", new EId("z"), new EPlus(new EId("y"), new EId("x")));
    System.out.println(test2.visit(new UnboundIdsVisitor()));
    Expr test3 = new ELet("x", new EId("z"), new EMinus(new EId("y"), new EId("x")));
    System.out.println(test3.visit(new UnboundIdsVisitorMinus()));
    Expr test4 = new ELet("x", new EId("z"), new EMinus(new EPlus(new EId("y"), new ENum(4)), new EId("x")));
    System.out.println(test4.visit(new UnboundIdsVisitorMinus()));
    Expr test5 = new ELet("x", new EId("z"), new EMinus(new ELam("w", new EPlus(new EId("y"), new EId("w"))), new EId("x")));
    System.out.println(test5.visit(new UnboundIdsVisitorMinusAndLam()));
  }
}