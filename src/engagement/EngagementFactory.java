package engagement;

import java.util.ArrayList;
import java.util.Iterator;

import troop.AbstractTroop;
import troop.Arrower;
import troop.Commander;
import troop.Sworder;
import troop.TroopBase;
import unit.Army;
import unit.ArrowerSquad;
import unit.Squad;
import unit.SworderSquad;

public class EngagementFactory<T extends Engagable, S>
{   
    public static final EngagementFactory<SworderSquad, Army> swordersquad = new EngagementFactory<SworderSquad, Army>();
    public static final EngagementFactory<ArrowerSquad, Army> arrowersquad = new EngagementFactory<ArrowerSquad, Army>();
    public static final EngagementFactory<Squad, Army> troopsquad = new EngagementFactory<Squad, Army>();
    public static final EngagementFactory<Sworder, Army> sworder = new EngagementFactory<Sworder, Army>();
    public static final EngagementFactory<Arrower, Army> arrower = new EngagementFactory<Arrower, Army>();
    public static final EngagementFactory<AbstractTroop, Army> troop = new EngagementFactory<AbstractTroop, Army>();

    private EngagementFactory()
    {
    }
    
    public ArrayList<Engagement<T, S>> engage(EngagableGroup<T> g1, EngagableGroup<T> g2, S side1, S side2)
    {
        return engage(g1, g2, new EngagableGroupWrapper<T>(), new EngagableGroupWrapper<T>(), side1, side2);
    }
    
    public ArrayList<Engagement<T, S>> engage(EngagableGroup<T> e1, EngagableGroup<T> e2, EngagableGroup<T> aux1, EngagableGroup<T> aux2, S side1, S side2)
    {
        ArrayList<EngagableGroup<T>> g1s = new ArrayList<EngagableGroup<T>>();
        ArrayList<EngagableGroup<T>> g2s = new ArrayList<EngagableGroup<T>>();
        ArrayList<EngagableGroup<T>> aux1s = new ArrayList<EngagableGroup<T>>();
        ArrayList<EngagableGroup<T>> aux2s = new ArrayList<EngagableGroup<T>>();
        return engage(g1s, g2s, aux1s, aux2s, side1, side2);
    }
    
    public ArrayList<Engagement<T, S>> engage(ArrayList<EngagableGroup<T>> g1s, ArrayList<EngagableGroup<T>> g2s, S side1, S side2)
    {
        ArrayList<EngagableGroup<T>> aux1s = new ArrayList<EngagableGroup<T>>();
        ArrayList<EngagableGroup<T>> aux2s = new ArrayList<EngagableGroup<T>>();
        return engage(g1s, g2s, aux1s, aux2s, side1, side2);
    }
    
    /**
     * Assigns each member of each group in <g1s> to the corresponding (by index) member of <g2s> as an Engagement.
     * If there are more members of one group than the other, they will be added to an existing
     * Engagement, starting with the Engagement in index 0 and adding one member to each Engagement.
     * If all Engagements are assigned a third member, the process repeats assigning a fourth member
     * to each Engagement, etc. until all members have been assigned to an Engagement.
     * Each Engagement will remember the EngagableGroup each member originated from.
     * @param g1 Group of members to be engaged
     * @param g2 Group of members to be engaged
     * @return A Collection of all Engagements produced by the above process
     */
    public ArrayList<Engagement<T, S>> engage(ArrayList<EngagableGroup<T>> g1s, ArrayList<EngagableGroup<T>> g2s,
            ArrayList<EngagableGroup<T>> aux1s, ArrayList<EngagableGroup<T>> aux2s, S side1, S side2)
    {
        // TODO determine the edge case for if all commanders are dead
        if(g1s.size() == 0 && g2s.size() == 0) return null;
        
        ArrayList<Engagement<T, S>> eng = new ArrayList<Engagement<T, S>>();

        int i = 0;
        int j = 0;

        Iterator<T> iter1 = g1s.get(i).iterator();
        Iterator<T> iter2 = g2s.get(j).iterator();

        while(i < g1s.size() && j < g2s.size())
        {
            iter1 = g1s.get(i).iterator();
            iter2 = g2s.get(j).iterator();

            if(iter1.hasNext() || iter2.hasNext())
            {
                if(iter1.hasNext() && iter2.hasNext())
                {
                    // members remain in both groups
                    // pair members from each group
                    eng.add(new Engagement<T, S>(iter1.next(), iter2.next(),
                            g1s.get(i), g2s.get(j), side1, side2));
                } else
                {
                    // one group is out of members
                    // switch to the next group from that side
                    if(iter1.hasNext()) j++;
                    else i++;
                    continue;
                }
            } else
            {
                // both iterators are empty so increment to next groups for both sides
                i++;
                j++;
            }
        }
     
        // one side is out of members
        if(i < g1s.size())
        {
            eng = engageAux(eng, j, iter2, g2s, aux1s, side2, side1);
        } else
        {
            eng = engageAux(eng, i, iter1, g1s, aux2s, side1, side2);
        }
        
        return eng;
    }
    
    private ArrayList<Engagement<T, S>> engageAux(ArrayList<Engagement<T, S>> eng, int side_index, Iterator<T> current_group,
            ArrayList<EngagableGroup<T>> side_groups, ArrayList<EngagableGroup<T>> aux_groups,
            S group_side, S aux_side)
    {
        // one side switches to aux groups
        int aux_index = 0; // this now indexes aux groups for side1
        Iterator<T> aux_iter = aux_groups.get(aux_index).iterator();
        while(true)
        {
            if(!current_group.hasNext())
            {
                side_index++;
                if(side_index < side_groups.size()) current_group = side_groups.get(side_index).iterator();
                else break;
            }
            
            if(!aux_iter.hasNext())
            {
                aux_index++;
                if(aux_index < aux_groups.size()); // do nothing
                else aux_index = 0;
                aux_iter = aux_groups.get(aux_index).iterator();
            }
            
            eng.add(new Engagement<T, S>(current_group.next(), aux_iter.next(),
                    side_groups.get(side_index), aux_groups.get(aux_index), group_side, aux_side));
        }
        
        return eng;
    }
}
