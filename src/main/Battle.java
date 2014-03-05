package main;

import java.util.ArrayList;
import java.util.Iterator;

import engagement.EngagableGroup;
import engagement.EngagableGroupWrapper;
import engagement.Engagement;
import engagement.EngagementFactory;
import troop.AbstractTroop;
import troop.Arrower;
import troop.Sworder;
import troop.SworderCommander;
import unit.Army;
import unit.ArrowerSquad;
import unit.ScatteredSquad;
import unit.Squad;
import unit.SworderSquad;

public class Battle
{
    public static final int MODE_START = 0;
    public static final int MODE_SVS = 1;
    public static final int MODE_SVA = 2;
    public static final int MODE_AVA = 3;
    public static final int MODE_END = -1;

    private int mode = MODE_START;

    Army a1;
    Army a2;
    ArrayList<Engagement<SworderSquad, Army>> squad_eng = new ArrayList<Engagement<SworderSquad, Army>>();
    ArrayList<SworderSquad> squad_uneng_a1 = new ArrayList<SworderSquad>();
    ArrayList<SworderSquad> squad_uneng_a2 = new ArrayList<SworderSquad>();

    public Battle(Army a1, Army a2)
    {
        this.a1 = a1;
        this.a2 = a2;
    }

    public boolean performTurn()
    {
        switch (mode)
        {
            case MODE_START:
                squad_uneng_a1.addAll(a1.sworders);
                squad_uneng_a2.addAll(a2.sworders);
                mode = MODE_SVS;
                performSVS();
            break;
            case MODE_SVS:
                performSVS();
            break;
            case MODE_SVA:
                performSVA();
            break;
            case MODE_AVA:
                performAVA();
            break;
            case MODE_END:
                return true;
        }
        
        return false;
    }

    public void performSVS()
    {
        // arrowers attack
        performArrowize(a1, a2);
        performArrowize(a2, a1);

        // All sworders are dead
        if(a1.sworders.isEmpty() && a2.sworders.isEmpty())
        {
            mode = MODE_AVA; // archer only combat
            return;
        }

        // one side's sworders are dead
        if(a1.sworders.isEmpty() || a2.sworders.isEmpty())
        {
            mode = MODE_SVA; // sworder versus archer slaughter
            squad_uneng_a1.clear();
            squad_uneng_a2.clear();
            
            squad_uneng_a1.addAll(a1.sworders);
            squad_uneng_a1.addAll(a1.scattered);
            
            squad_uneng_a2.addAll(a2.sworders);
            squad_uneng_a2.addAll(a2.scattered);
            
            return;
        }

        // free squads may rally scattered sworders
        for(SworderSquad s: squad_uneng_a1)
        {
            while(s.numTroops() < s.getCommander().getCommand())
            {
                s.addTroop(a1.scattered.get(a1.scattered.size() - 1).pop());
                removeLastScatteredSquadIfEmpty(a1);
            }
        }

        for(SworderSquad s: squad_uneng_a2)
        {
            while(s.numTroops() < s.getCommander().getCommand())
            {
                s.addTroop(a2.scattered.get(a2.scattered.size() - 1).pop());
                removeLastScatteredSquadIfEmpty(a2);
            }
        }

        // Engage Squads
        ArrayList<SworderSquad> ss1 = new ArrayList<SworderSquad>();
        Iterator<ScatteredSquad> iter = a1.scattered.iterator();
        while(iter.hasNext())
            ss1.add(iter.next());

        ArrayList<SworderSquad> ss2 = new ArrayList<SworderSquad>();
        iter = a2.scattered.iterator();
        while(iter.hasNext())
            ss2.add(iter.next());

        squad_eng.addAll(EngagementFactory.swordersquad.engage(
                new EngagableGroupWrapper<SworderSquad>(squad_uneng_a1),
                new EngagableGroupWrapper<SworderSquad>(squad_uneng_a2),
                new EngagableGroupWrapper<SworderSquad>(ss1),
                new EngagableGroupWrapper<SworderSquad>(ss2),
                a1, a2));

        // combat!
        for(Engagement<SworderSquad, Army> squadVsquad: squad_eng)
        {
            // get sworders by army
            ArrayList<EngagableGroup<Sworder>> a1_swd_sqds = new ArrayList<EngagableGroup<Sworder>>();
            a1_swd_sqds.addAll(squadVsquad.getEngagablesBySide(a1));

            ArrayList<EngagableGroup<Sworder>> a2_swd_sqds = new ArrayList<EngagableGroup<Sworder>>();
            a2_swd_sqds.addAll(squadVsquad.getEngagablesBySide(a1));

            ArrayList<Engagement<Sworder, Army>> sworderVsworder = EngagementFactory.sworder
                    .engage(a1_swd_sqds, a2_swd_sqds, a1, a2);

            for(Engagement<Sworder, Army> skirmish: sworderVsworder)
            {
                // each member of each group receives queued damage from members
                // of groups from other sides
                ArrayList<EngagableGroup<Sworder>> squads = skirmish
                        .getGroups();
                for(EngagableGroup<Sworder> squad: squads)
                {
                    queueDamageForSworderSquad((SworderSquad) squad, skirmish);
                }

                // deal damage simultaneously; remove dead sworders
                damageAndKillSworders(squads, skirmish);

                // check for rout
                for(EngagableGroup<Sworder> squad: squads)
                {
                    SworderCommander sc1 = (SworderCommander) ((SworderSquad) squad)
                            .getCommander();
                    if(sc1.getStrength() < 1)
                    {
                        routSquad((SworderSquad) squad, skirmish);
                    }
                }
            }
        }
    }

    public void performSVA()
    {
        Army swd_army;
        ArrayList<SworderSquad> swd_sqds;
        ArrayList<ArrowerSquad> arr_sqds;
        ArrayList<ArrowerSquad> arr_sqds_ally;
        if(a1.sworders.isEmpty() && a1.scattered.isEmpty())
        {
            swd_army = a2;
            swd_sqds = squad_uneng_a2;
            arr_sqds = a1.arrowers;
            arr_sqds_ally = a2.arrowers;
        }
        else
        {
            swd_army = a1;
            swd_sqds = squad_uneng_a1;
            arr_sqds = a2.arrowers;
            arr_sqds_ally = a1.arrowers;
        }
        
        if(swd_sqds.isEmpty())
        {
            mode = MODE_AVA;
            performAVA();
        }
        
        for(SworderSquad swd_sqd: swd_sqds)
        {
            // kill arrowers proportional to strength
            for(Sworder swd: swd_sqd.getAll())
            {
                killArrowers(arr_sqds, (swd.getStrength()/1000)+1);
            }
            
            // suffer damage from surviving arrowers
            for(Sworder swd: swd_sqd.getAll())
            {
                for(ArrowerSquad arr_sqd: arr_sqds)
                {
                    for(Arrower arr: arr_sqd.getAll())
                    {
                        swd.sufferDamage(arr.getStrength());
                    }
                }
            }
            
            // arrower barrage from the sworders' allies
            for(ArrowerSquad arr_sqd: arr_sqds)
            {
                Engagement<AbstractTroop,>
                sufferDamageFromArrowers(arr_sqd, arr_sqds_ally);
            }
        }
    }

    public void performAVA()
    {

    }
    
    public void killArrowers(ArrayList<ArrowerSquad> arr_sqds, int quant)
    {
        while(quant > 0)
        {
            ArrowerSquad arr_sqd = arr_sqds.get(arr_sqds.size() - 1);
            arr_sqd.removeTroop(arr_sqd.size() - 1);
        }
    }

    public void performArrowize(Army arr_side, Army swd_side)
    {
        ArrayList<Squad> arr_squads = new ArrayList<Squad>();
        for(ArrowerSquad arr: arr_side.arrowers)
        { 
            arr_squads.add(arr);
        }
        
        ArrayList<Squad> swd_squads = new ArrayList<Squad>();
        for(SworderSquad swd: swd_side.sworders)
        {
            swd_squads.add(swd);
        }
        
        ArrayList<Engagement<Squad, Army>> volleys = EngagementFactory.troopsquad.engage(
                new EngagableGroupWrapper<Squad>(arr_squads),
                new EngagableGroupWrapper<Squad>(swd_squads),
                arr_side, swd_side);
        
        // volleys = squadVsquad
        // hail = arrowerVsworder
        for(Engagement<Squad, Army> volley: volleys)
        {
            for(Squad s_squad: volley.getEngagablesBySide(swd_side))
            {
                ArrayList<Squad> a_squads = volley.getEngagablesBySide(arr_side);
                ArrayList<AbstractTroop> tmp_arrlist = new ArrayList<AbstractTroop>();
                for(Squad s: a_squads)
                {
                    tmp_arrlist.addAll(s.getAll());
                }
                
                ArrayList<Engagement<AbstractTroop, Army>> hails = EngagementFactory.troop.engage(
                        new EngagableGroupWrapper<AbstractTroop>(tmp_arrlist), s_squad,
                        arr_side, swd_side);
                
                for(Engagement<AbstractTroop, Army> hail: hails)
                {
                    sufferDamageFromArrowers(s_squad, hail);
                }
            }
        }
        
        ArrayList<EngagableGroup<Sworder>> tmp_swd_sqd = new ArrayList<EngagableGroup<Sworder>>();
        tmp_swd_sqd.addAll(swd_side.sworders);
        damageAndKillSworders(tmp_swd_sqd, null);
        
        for(EngagableGroup<Sworder> squad: swd_side.sworders)
        {
            SworderCommander sc1 = (SworderCommander) ((SworderSquad) squad)
                    .getCommander();
            if(sc1.getStrength() < 1)
            {
                dissolveSquad((SworderSquad) squad, swd_side);
            }
        }
    }

    public void damageAndKillSworders(ArrayList<EngagableGroup<Sworder>> squads, Engagement<Sworder, Army> skirmish)
    {
        damageAndKillSworders(squads, skirmish);
    }
    
    public void damageAndKillSworders(EngagableGroup<Sworder> squad, Engagement<Sworder, Army> skirmish)
    {
        for(Sworder s: squad.getAll())
        {
            s.sufferQueuedDamage();
            if(s.getStrength() <= 0)
            {
                if(skirmish != null) skirmish.remove(s);
                squad.remove(s);
            }
        }
    }
    
    public void dissolveSquad(SworderSquad squad, Army side)
    {
        for(Sworder s: squad.getTroops())
        {
            ScatteredSquad scats = side.scattered.get(side.scattered.size() - 1);
            if(scats.size() < 4) scats.addTroop(s);
            else
            {
                scats = new ScatteredSquad();
                scats.addTroop(s);
                side.scattered.add(scats);
            }
        }
    }
    
    public void routSquad(SworderSquad squad, Engagement<Sworder, Army> skirmish)
    {
        queueDamageForSworderSquad(squad, skirmish);
        damageAndKillSworders(squad, skirmish);
        Army side = skirmish.getSideOfGroup(squad);
        dissolveSquad(squad, side);
    }

    public void queueDamageForSworderSquad(EngagableGroup<Sworder> squad,
            Engagement<Sworder, Army> skirmish)
    {
        // each member of squad suffers damage from the opposing squad(s)
        Iterator<Sworder> iter1 = squad.iterator();
        while(iter1.hasNext())
        {
            Sworder s1 = iter1.next();
            for(EngagableGroup<Sworder> squad2: skirmish.getOtherGroups(squad))
            {
                Iterator<Sworder> iter2 = squad2.iterator();
                while(iter2.hasNext())
                {
                    Sworder s2 = iter2.next();
                    s1.queueDamage(s2.getStrength() / squad2.size());
                }
            }
        }
    }
    
    public void sufferDamageFromArrowers(EngagableGroup<AbstractTroop> squad,
            Engagement<AbstractTroop, Army> hail)
    {
        // each member of squad suffers damage from the opposing squad(s)
        Iterator<AbstractTroop> iter1 = squad.iterator();
        while(iter1.hasNext())
        {
            AbstractTroop s1 = iter1.next();
            for(EngagableGroup<AbstractTroop> squad2: hail.getOtherGroups(squad))
            {
                Iterator<AbstractTroop> iter2 = squad2.iterator();
                while(iter2.hasNext())
                {
                    AbstractTroop s2 = iter2.next();
                    s1.sufferDamage(s2.getStrength());
                }
            }
        }
    }

    public void removeLastScatteredSquadIfEmpty(Army army)
    {
        if(army.scattered.get(army.scattered.size()).size() == 0)
        {
            army.scattered.remove(army.scattered.size());
        }
    }

    public void printArmies()
    {
        System.out.println("Army 1:");
        a1.print();
        System.out.println("\n---------\n");
        System.out.println("Army 2:");
        a2.print();
    }
}
