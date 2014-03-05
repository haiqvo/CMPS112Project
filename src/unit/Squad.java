package unit;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import engagement.EngagableGroup;
import troop.AbstractTroop;

abstract public class Squad<T extends AbstractTroop, C extends T> implements EngagableGroup<T>
{   
    C c;
    List<T> troops;
    
    private String squad_id_str;

    // Squad does not yet limit construction to a legal configuration
    // not relevant until we make the army creation module
    public Squad(C c, Collection<T> troops)
    {
        this.c = c;
        this.troops.addAll(troops);
    }

    @Override
    public T get(int index)
    {
        if(index < troops.size()) return troops.get(index);
        else return c;
    }
    
    @Override
    public Collection<T> getAll()
    {
        ArrayList<T> out = new ArrayList<T>();
        out.addAll(troops);
        out.add(c);
        return out;
    }
    
    public Collection<T> getTroops()
    {
        ArrayList<T> out = new ArrayList<T>();
        out.addAll(troops);
        return out;
    }
    
    @Override
    public int size()
    {
        if(c.getStrength() > 0) return troops.size() + 1;
        return troops.size();
    }
    
    public int numTroops()
    {
        return troops.size();
    }
    
    public C getCommander()
    {
        return c;
    }

    @Override
    public Iterator<T> iterator()
    {
        List<T> out = new LinkedList<T>(troops);
        if(c.getStrength() > 0) out.add(c);
        return out.iterator();
    }
    
    public void addTroop(T t)
    {
        troops.add(t);
    }
    
    public T removeTroop(int index)
    {
        return troops.remove(index);
    }
    
    public boolean remove(T e)
    {
        if(troops.remove(e)) return true;
        else
        {
            if(c == e)
            {
                //TODO determine behavior for removing a commander
                //c = null;
                //return true;
            }
            return false;
        }
    }
    
    /**
     * Prints the commander in the form (command, strength)
     * and the array of strengths of troops in the squad.
     * These values are labeled with the name "Commander"
     * and the name of the troop type.
     */
    public void print()
    {
        if(c.getStrength() > 0) System.out.println("Commander: "+c);
        else System.out.println("Commander: DEAD");
        System.out.print(" "+squad_id_str+": ");
        printTroops();
    }
    
    /**
     * Prints only the array of troops with no labels.
     */
    public void printTroops()
    {
        System.out.print("[");
        for(Iterator<T> t = troops.iterator(); true; t.next())
        {
            System.out.print(t);
            if(t.hasNext()) System.out.print(", ");
            else break;
        }
        System.out.println("]");
    }
}
