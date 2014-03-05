package engagement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;

import unit.Squad;

public class Engagement<T extends Engagable, S>
{
    HashMap<EngagableGroup<T>, ArrayList<T>> eng = new HashMap<EngagableGroup<T>, ArrayList<T>>();
    HashMap<S, ArrayList<EngagableGroup<T>>> side2group = new HashMap<S, ArrayList<EngagableGroup<T>>>();
    
    /**
     * This constructor is rarely used. More likely you will want to use the static EngagementFactory.engage()
     * @param e1
     * @param e2
     */
    public Engagement(T e1, T e2, EngagableGroup<T> g1, EngagableGroup<T> g2, S side1, S side2)
    {
        addEngaged(g1, e1, side1);
        addEngaged(g2, e2, side2);
    }
    
    public HashMap<EngagableGroup<T>, ArrayList<T>> getEngaged()
    {
        return eng;
    }
    
    public EngagableGroup<T> getGroupByIndex(int index)
    {
        Iterator<EngagableGroup<T>> g_iter = eng.keySet().iterator();
        
        for(int i = 0; i < index; i++)
        {
            g_iter.next();
        }
        
        return g_iter.next();
    }
    
    public T getEngagedByIndexInGroup(EngagableGroup<T> g, int index)
    {
        return eng.get(g).get(index);
    }
    
    public ArrayList<T> getEngagedByGroup(EngagableGroup<T> g)
    {
         return eng.get(g);
    }
    
    public ArrayList<T> getEngagedByGroupIndex(int index)
    {
        return eng.get(eng.keySet().toArray()[index]);
    }
    
    /** 
     * @param e engagement who's group is desired
     * @return the EngagementGroup associated with <e>, or <null> if it is not found
     */
    public EngagableGroup<T> getGroupOfEngaged(T e)
    {
        Set<Entry<EngagableGroup<T>,ArrayList<T>>> map = eng.entrySet();
        
        for(Entry<EngagableGroup<T>,ArrayList<T>> ent: map)
        {
            if(ent.getValue().contains(e)) return ent.getKey();
        }
        
        return null;
    }
    
    public ArrayList<EngagableGroup<T>> getGroupsWithoutEngaged(T e)
    {
        ArrayList<EngagableGroup<T>> out = new ArrayList<EngagableGroup<T>>();
        for(EngagableGroup<T> g: eng.keySet())
        {
            if(eng.get(g).contains(e))
            {
                out.add(g);
            }
        }
        
        return out;
    }
    
    public ArrayList<EngagableGroup<T>> getOtherGroups(EngagableGroup<T> e)
    {
        ArrayList<EngagableGroup<T>> out = new ArrayList<EngagableGroup<T>>();
        for(EngagableGroup<T> g: eng.keySet())
        {
            if(g != e)
            {
                out.add(g);
            }
        }
        
        return out;
    }
    
    public ArrayList<EngagableGroup<T>> getGroups()
    {
        ArrayList<EngagableGroup<T>> out = new ArrayList<EngagableGroup<T>>();
        for(EngagableGroup<T> g: eng.keySet())
        {
            out.add(g);
        }
        
        return out;
    }
    
    public boolean contains(T e)
    {
        for(ArrayList<T> v: eng.values())
        {
            if(v.contains(e)) return true;
        }
        return false;
    }
    
    public boolean remove(T e)
    {
        for(ArrayList<T> v: eng.values())
        {
            if(v.contains(e))
            {
                return v.remove(e);
            }
        }
        return false;
    }
    
    public S getSideOfGroup(EngagableGroup<T> group)
    {
        for(S side: side2group.keySet())
        {
            if(side2group.get(side) == group) return side;
        }
        
        return null;
    }
    
    public ArrayList<EngagableGroup<T>> getGroupsBySide(S side)
    {
        return side2group.get(side);
    }
    
    public ArrayList<T> getEngagablesBySide(S side)
    {
        ArrayList<T> out = new ArrayList<T>();
        for(EngagableGroup<T> group: side2group.get(side))
        {
            out.addAll(group.getAll());
        }
        return out;
    }
    
    public void addEngaged(EngagableGroup<T> g, T e, S side)
    {
        ArrayList<T> list_single = eng.get(g);
        if(list_single == null) list_single = new ArrayList<T>();
        else
        {
            list_single.add(e);
            eng.put(g, list_single);
        }
        
        ArrayList<EngagableGroup<T>> list_group = side2group.get(side);
        if(list_group == null) list_group = new ArrayList<EngagableGroup<T>>();
        else
        {
            list_group.add(g);
            side2group.put(side, list_group);
        }
    }
}
