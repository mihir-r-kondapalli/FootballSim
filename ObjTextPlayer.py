from ObjGame import Game
from dbai import DBAI

import numpy as np

# 0 for cpu vs cpu, 1 for player vs cpu, 2 for player vs player
print()
num_plays = int(input('Number of plays -> '))
print()

class Human:
    def intrct(game):
        global playing
        
        valid = False
        choice = 0
        while not(valid):
            intext = input(str(game.pos.name)+': 1 to run, 2 to pass, 3 for fg, 4 to punt -> ')
            if intext.isnumeric():
                choice = int(intext)
                if choice==1 or choice==2 or (choice==3 and game.field.loc<=50) or choice==4:
                    valid = True
                elif choice==0:
                    Playing = False
                    valid = True
                    print('END OF GAME')
                elif choice == 3 and game.field.loc>50:
                    print('Field Goals can only be attempted from the 50 yard line or closer.')
                else:
                    print('Please enter a valid number.')
        
        return choice

    def xp_intrct(game):
        playstr = game.pos.name
        
        valid = False
        choice = 0
        
        while not(valid):
            intext = input(playstr+': 1 for xp try, 2 for 2pt try -> ')
            if intext.isnumeric():
                choice = int(intext)
                if choice==1 or choice==2:
                    valid = True
                else:
                    print('Please enter a valid number.')
                
        if choice == 1:
            #if game.get_dice_val() > 1:
            if np.random.uniform(0, 1) <= 0.945:
                print()
                print('---------XP IS MADE!---------')
                game.update_score(game.XP)
                print(game.get_score_str())
                print()
            else:
                print()
                print('---------XP IS MISSED!---------')
                print(game.get_score_str())
                print()
        if choice == 2:
            #if game.get_dice_val() > 4:
            if np.random.uniform(0, 1) <= 0.45:
                print()
                print('---------2PT TRY IS GOOD!---------')
                game.update_score(game.TPT)
                print(game.get_score_str())
                print()
            else:
                print()
                print('---------2PT TRY IS FAILED!---------')
                print(game.get_score_str())
                print()

class Computer:
    def __init__(self, ai):
        self.ai = ai
    def intrct(self, game):
        if not(SIM):
            cont()
        return self.ai.get_opt_play(game.field.down, game.field.get_distance(), game.field.loc)
    def xp_intrct(self, game):
        if not(SIM):
            cont()
        if np.random.uniform(0, 1) <= 0.945:
            print()
            print('---------XP IS MADE!---------')
            game.update_score(game.XP)
            print(game.get_score_str())
            print()
        else:
            print()
            print('---------XP IS MISSED!---------')
            print(game.get_score_str())
            print()

def update_score(game, score_type, player=None):
    if(score_type == 'TD'):
        game.update_score(game.TD)
        print(game.get_score_str())
        player.xp_intrct(game)
    if(score_type == 'FG'):
        game.update_score(game.FG)
        print(game.get_score_str())
    if(score_type == 'SFT'):
        game.update_score(game.SFT)
        print(game.get_score_str())

def cont():
    global playing
    print()
    answer = input('Press enter to continue')
    print()
    if answer=='0':
        playing = False

drive_results = []
names = []
score1s = []
score2s = []
times = []

def log(text, name, time, score1, score2):
    drive_results.append(text)
    names.append(name)
    score1s.append(score1)
    score2s.append(score2)
    times.append(time)

def log_summary():
    print()
    print()
    print('DRIVE SUMMARY')
    print()
    print('##| '+'%3d'%num_plays+' (--) | -SCORE- | -----------------------------')
    for i in range(0, len(drive_results)):
        tdiff = 0
        if i==0:
            tdiff = num_plays-times[i]
        else:
            tdiff = times[i-1]-times[i]
        print('%2d'%(i+1)+'| '+'%3d'%times[i]+' ('+'%2d'%tdiff+') | '+'%2d'%score1s[i]+' - '+'%2d'%score2s[i]+' | '+names[i]+': '+drive_results[i]) 
    print('--| --- (--) | '+'%2d'%score1s[i]+' - '+'%2d'%score2s[i]+' | -----------------------------')

def end_summary():
    t1 = game.team1.stats
    t2 = game.team2.stats

    print('Final Score:', game.get_score_str())
    print()
    print(f'Total Yards: ({t1.run_yds + t1.pass_yds} - {t2.run_yds + t2.pass_yds})')
    print(f'Total Plays: ({t1.runs + t1.passes} - {t2.runs + t2.passes})')

    # Ensure yards/play appears as a decimal
    ypp1 = (t1.run_yds + t1.pass_yds) / (t1.runs + t1.passes) if (t1.runs + t1.passes) > 0 else 0.00
    ypp2 = (t2.run_yds + t2.pass_yds) / (t2.runs + t2.passes) if (t2.runs + t2.passes) > 0 else 0.00
    print(f'Yards/Play: ({ypp1:.2f} - {ypp2:.2f})')

    print()
    print(f'Run Yards: ({t1.run_yds} - {t2.run_yds})')
    print(f'Run Plays: ({t1.runs} - {t2.runs})')

    # Ensure running avg appears as a decimal
    run_avg1 = t1.run_yds / t1.runs if t1.runs > 0 else 0.00
    run_avg2 = t2.run_yds / t2.runs if t2.runs > 0 else 0.00
    print(f'Running Avg: ({run_avg1:.2f} - {run_avg2:.2f})')

    print()
    print(f'Pass Yards: ({t1.pass_yds} - {t2.pass_yds})')
    print(f'Pass Plays: ({t1.passes} - {t2.passes})')

    # Ensure passing avg appears as a decimal
    pass_avg1 = t1.pass_yds / t1.passes if t1.passes > 0 else 0.00
    pass_avg2 = t2.pass_yds / t2.passes if t2.passes > 0 else 0.00
    print(f'Passing Avg: ({pass_avg1:.2f} - {pass_avg2:.2f})')

    print(f'Completions: ({t1.comps} - {t2.comps})')
    incompletions1 = t1.passes - t1.comps
    incompletions2 = t2.passes - t2.comps
    print(f'Incompletions: ({incompletions1} - {incompletions2})')

    # Ensure completion % appears as a decimal
    comp_pct1 = (t1.comps / t1.passes) * 100 if t1.passes > 0 else 0.00
    comp_pct2 = (t2.comps / t2.passes) * 100 if t2.passes > 0 else 0.00
    print(f'Completion %: ({comp_pct1:.2f}% - {comp_pct2:.2f}%)')

    print()
    print(f'Sacks Taken: ({t1.sacks} - {t2.sacks})')
    print(f'Fumbles: ({t1.fumbles} - {t2.fumbles})')
    print(f'Ints Thrown: ({t1.ints} - {t2.ints})')
    print(f'Total Punts: ({t1.punts} - {t2.punts})')
    print(f'First Downs: ({t1.fds} - {t2.fds})')

ai_bia = DBAI("biased/max_eps.csv", "biased/opt_choices.csv")
ai_unb = DBAI("unbiased/max_eps.csv", "unbiased/opt_choices.csv")
ai_nfl = DBAI("nflep/nfl_pbp_data.csv", "nflep/opt_choices.csv")

game = Game("ME", "BIA", num_plays) 
SIM = False
EP = True

playing = True

#player1 = Computer(ai_bia)
player1 = Human
player2 = Computer(ai_bia)

game.toss()
print(game.pos.name + " won the toss!\n")

drive = False

curr_player = None

while(playing):

    if(game.pos.name==game.team1.name):
        curr_player = player1
    else:
        curr_player = player2
    
    if not(drive):
        game.kickoff()
        print()
        print('Recieved ' + game.get_ball_pos_str(False))
        print()
        drive = True
        if not(SIM):
            cont()
    else:
        printstr = ''
    
        print()
        print(game.get_status_str())
        print()
        
        if(EP):
            epstr = "Unbiased EP: " + str(ai_unb.get_ep(game.field.down, game.field.get_distance(), game.field.loc))
            epstr += ", Biased EP: " + str(ai_bia.get_ep(game.field.down, game.field.get_distance(), game.field.loc))
            epstr += ", NFL EP: " + str(ai_nfl.get_ep(game.field.down, game.field.get_distance(), game.field.loc))
            print(epstr)

        # Choice
        choice = curr_player.intrct(game)
        
        print("\n")
        istd = False
        issft = False
        
        if choice == 1:
            play, result, yards = game.run()
            if(play == 'run'):
                print("Run for "+str(yards)+" yards.")
                if(result == 'first down'):
                    print("FIRST DOWN!")
                elif(result == 'touchdown'):
                    print("-------------TOUCHDOWN!-------------")
                    update_score(game, "TD", player=curr_player)
                    log("TOUCHDOWN", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                elif(result == 'turnover on downs'):
                    print("-------------TURNOVER ON DOWNS!-------------")
                    log("TURNOVER ON DOWNS", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
                elif(result == 'safety'):
                    print("-------------SAFETY!-------------")
                    game.switch_pos()
                    update_score(game, "SFT")
                    game.switch_pos()
                    log("SAFETY", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
            elif(play=='rfumble'):
                print("-------------FUMBLE!-------------")
                game.switch_pos()
                if(result == 'touchdown'):
                    print("TOUCHBACK.")
                    game.touchback()
                    game.switch_pos()
                    log("FUMBLE", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                elif(result == 'safety'):
                    print("-------------Fumble returned for a TOUCHDOWN!-------------")
                    update_score(game, "TD", player=curr_player)
                    game.switch_pos()
                    log("FUMBLE (TD)", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.switch_pos()
                    drive = False
                else:
                    print("Fumble returned for "+str(yards)+" yards.")
                    game.switch_pos()
                    log("FUMBLE", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
        
        elif choice == 2:
            play, result, yards = game.throw()
            if(play == 'pass'):
                print("Pass for "+str(yards)+" yards.")
                if(result == 'first down'):
                    print("FIRST DOWN!")
                elif(result == 'touchdown'):
                    print("-------------TOUCHDOWN!-------------")
                    update_score(game, "TD", player=curr_player)
                    log("TOUCHDOWN", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                elif(result == 'turnover on downs'):
                    print("-------------TURNOVER ON DOWNS!-------------")
                    log("TURNOVER ON DOWNS", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
                elif(result == 'safety'):
                    print("-------------SAFETY!-------------")
                    game.switch_pos()
                    update_score(game, "SFT")
                    game.switch_pos()
                    log("SAFETY", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
            elif(play=='pfumble'):
                print("-------------FUMBLE!-------------")
                game.switch_pos()
                if(result == 'touchdown'):
                    print("TOUCHBACK.")
                    game.touchback()
                    game.switch_pos()
                    log("FUMBLE", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                elif(result == 'safety'):
                    print("-------------Fumble returned for a TOUCHDOWN!-------------")
                    update_score(game, "TD", player=curr_player)
                    game.switch_pos()
                    log("FUMBLE", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.switch_pos()
                    drive = False
                else:
                    print("Fumble returned for "+str(yards)+" yards.")
                    game.switch_pos()
                    log("FUMBLE", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
            elif(play=='int'):
                print("-------------INTERCEPTION!-------------")
                game.switch_pos()
                if(result == 'touchdown'):
                    print("TOUCHBACK.")
                    game.touchback()
                    game.switch_pos()
                    log("INTERCEPTION", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = True
                elif(result == 'safety'):
                    print("-------------Interception returned for a TOUCHDOWN!-------------")
                    update_score(game, "TD", player=curr_player)
                    game.switch_pos()
                    log("INTERCEPTION (TD)", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.switch_pos()
                    drive = False
                else:
                    print("Interception returned for "+str(yards)+" yards.")
                    game.switch_pos()
                    log("INTERCEPTION", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
            elif(play=='inc'):
                print("Pass incomplete.")
                if(result == 'turnover on downs'):
                    print("-------------TURNOVER ON DOWNS!-------------")
                    log("TURNOVER ON DOWNS", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
            elif(play == 'scramble'):
                print("Scramble for "+str(yards)+" yards.")
                if(result == 'first down'):
                    print("FIRST DOWN!")
                elif(result == 'touchdown'):
                    print("-------------TOUCHDOWN!-------------")
                    update_score(game, "TD", player=curr_player)
                    log("TOUCHDOWN", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                elif(result == 'turnover on downs'):
                    print("-------------TURNOVER ON DOWNS!-------------")
                    log("TURNOVER ON DOWNS", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
                elif(result == 'safety'):
                    print("-------------SAFETY!-------------")
                    game.switch_pos()
                    update_score(game, "SFT")
                    game.switch_pos()
                    log("SAFETY", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
            elif(play == 'sack'):
                print("SACK TAKEN for a loss of "+str(abs(yards))+" yards.")
                if(result == 'first down'):
                    print("FIRST DOWN!")
                elif(result == 'touchdown'):
                    print("-------------TOUCHDOWN!-------------")
                    update_score(game, "TD",)
                    log("TOUCHDOWN", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                elif(result == 'turnover on downs'):
                    print("-------------TURNOVER ON DOWNS!-------------")
                    log("TURNOVER ON DOWNS", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    game.new_downs()
                elif(result == 'safety'):
                    print("-------------SAFETY!-------------")
                    game.switch_pos()
                    update_score(game, "SFT")
                    game.switch_pos()
                    log("SAFETY", game.pos.name, game.time, game.team1.score, game.team2.score)
                    game.switch_pos()
                    drive = False
                
        elif choice == 3:
            result, yds = game.kick()
            print('KICK FOR '+str(yds)+' YARDS.')
            if result == 'made':
                print('---------FG IS MADE!---------')
                update_score(game, "FG")
                log("FG MADE", game.pos.name, game.time, game.team1.score, game.team2.score)
                game.switch_pos()
                drive = False
                print()
            else:
                print('---------FG IS MISSED!---------')
                log("FG MISSED", game.pos.name, game.time, game.team1.score, game.team2.score)
                game.switch_pos()
                game.new_downs()
                print()
        elif choice == 4:
            result, yds = game.punt()
            if result == 'touchback':
                log("PUNT", game.pos.name, game.time, game.team1.score, game.team2.score)
                game.switch_pos()
                game.touchback()
                print('PUNT FOR '+str(yds)+' YARDS.')
                print('TOUCHBACK.')
                print()
            elif result == 'touchdown':
                print('PUNT FOR '+str(yds)+' YARDS.')
                print('PUNT MUFFED!!!')
                print("-------------TOUCHDOWN!-------------")
                update_score(game, "TD", player=curr_player)
                log("MUFFED PUNT TOUCHDOWN", game.pos.name, game.time, game.team1.score, game.team2.score)
                game.switch_pos()
                drive = False
                print()
            elif result == 'muffed punt':
                game.new_downs()
                print('PUNT FOR '+str(yds)+' YARDS.')
                print('PUNT MUFFED!!!')
                print()
            elif result == 'return touchdown':
                game.switch_pos()
                print('---------PUNT RETURNED FOR A TOUCHDOWN!!!---------')
                print()
                update_score(game, "TD", player=curr_player)
                game.switch_pos()
                log("PUNT RETURNED FOR TOUCHDOWN", game.pos.name, game.time, game.team1.score, game.team2.score)
                game.switch_pos()
                game.switch_pos()
                drive = False
            else:
                log("PUNT", game.pos.name, game.time, game.team1.score, game.team2.score)
                game.switch_pos()
                game.new_downs()
                print('PUNT FOR '+str(yds)+' YARDS.')
                print()
            
        print()
        
    if game.is_done():
        playing = False


end_summary()
log_summary()