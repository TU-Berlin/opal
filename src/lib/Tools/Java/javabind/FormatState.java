package espress.util.java.format;

/**
  * A class which realizes a state-oriented frontend to
  * the formatting facilities. 
  *
  * @version $Id: FormatState.java,v 1.1 1999-10-16 17:32:05 wg Exp $
  * @author Wolfgang Grieskamp
  */

import java.util.Stack;
import java.util.Vector;
import java.io.PrintWriter;


public class FormatState {

  /** The current vector to append formats to. */
  private Vector formats = new Vector{};

  /** A stack of outer formats. */
  private Stack  stack = new Stack();

  /** Construct a new format state. */
  public FormatState(){}

  /** Append a literal to the format state. */
  public void lit(String lit){
    formats.addElement(new FormatString(lit));
  }

  /** Append a line break to the format state. */
  public void brk(){
    formats.addElement(new FormatBreak());
  }


  /** Append a space  to the format state. */
  public void spc(){
    formats.addElement(new FormatSpace());
  }

  /** Append a space  to the format state. */
  public void spc(int count){
    formats.addElement(new FormatSpace(count));
  }

  /** Begin a new nested block. */
  public void beg(int indent){
    stack.push(formats);
    formats = new Vector();
  }

  /** End a nested block. */
  public void end(){
    Format block = new FormatBlock((Format[])formats.toArray());
    formats = stack.pop();
    formats.addElement(block);
  }

  /** Return format in state. */
  public Format toFormat(){
    return new FormatBlock((Format[])formats.toArray());
  }

  /** Output format in state to a PrintWriter. */
  public void print(PrintWriter writer, int width){
    toFormat().printFormat(writer, width);
  }
    
}
