package engagement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class EngagableGroupWrapper<T extends Engagable> implements EngagableGroup<T>
{
    private ArrayList<T> engagables = new ArrayList<T>();
    
    public EngagableGroupWrapper()
    {   
    }
    
    public EngagableGroupWrapper(Collection<T> s)
    {
        engagables.addAll(s);
    }

    @Override
    public T get(int index)
    {
        return engagables.get(index);
    }
    
    @Override
    public Collection<T> getAll()
    {
        return engagables;
    }

    @Override
    public Iterator<T> iterator()
    {
        ArrayList<T> al = new ArrayList<T>();
        al.addAll(engagables);
        return al.iterator();
    }
    
    @Override
    public int size()
    {
        return engagables.size();
    }

    @Override
    public boolean remove(T e)
    {
        return engagables.remove(e);
    }
}
