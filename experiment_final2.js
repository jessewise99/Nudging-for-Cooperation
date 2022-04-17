/* initialize jsPsych */
var jsPsych = initJsPsych({
    on_finish: function () {
        jsPsych.data.addProperties({
            framing: framing_idx,
            balance: balance,
            bonus: balance/x_rate,
            prolific_id: jsPsych.data.getURLVariable("PROLIFIC_PID"),
        });
        var all_data = jsPsych.data.get().ignore({ type:'instructions' }).csv();
        console.log(all_data);
        go_back_to_prolific= true;
     
        // send data to server
        var id = jsPsych.data.getURLVariable("PROLIFIC_PID")
        save_data(id+"_data.csv", all_data);
        if (go_back_to_prolific) {
            document.location.href = "https://app.prolific.co/submissions/complete?cc=65D25CA9";
        }
    }
});


//Preloading Player images
var playerpics = ['player.png', 'player.png']
var sample_instructions_picture = ['sample_instructions.png']
var preload = {
    type: jsPsychPreload,
    images: playerpics.concat(sample_instructions_picture)
}
var DEMOGRAPHICS_HTML = `
<fieldset>
  <legend>Age</legend>
  <input type="text" id="age" name="age" required minlength=2 maxlength=2> years
</fieldset>
<fieldset>
  <legend>Gender</legend>
  <input type="radio" id="female" value="female" name="gender" required>
  <label for="female">Female</label><br>
  <input type="radio" id="male" value="male" name="gender" required>
  <label for="male">Male</label><br>
  <input type="radio" id="selfidentify" value="selfidentify" name="gender" required>
  <label for="selfidentify">Prefer to self-identify</label><br>
  <label for="selfidentifiedgender">I self-identify as </label>
  <input type="text" name="selfidentifiedgender" id="selfidentifiedgender" maxlength=35 disabled>
</fieldset>
<fieldset>
  <legend>Income</legend>
  <input type="radio" id="low" value="low" name="income" >
  <label for="low">Low (£0 - £10000 per year, including students and unemployed)</label><br>
  <input type="radio" id="middle" value="middle" name="income" >
  <label for="middle">Middle (£10001 - £30000)</label><br>
  <input type="radio" id="high" value="high" name="income" >
  <label for="high">High (£30001 and over)</label><br>
  <input type="radio" id="notDisclosed" value="notDisclosed" name="income" >
  <label for="notDisclosed">I would rather not say</label><br>
</fieldset>
`;

var go_back_to_prolific = false;
var OTHER_PLAYER_NAME = "Anonymous";
var OTHER_PLAYER_IMAGE = 'player.png';
var PLAYER_IMAGE = 'player.png';
var adj_sample_instructions_picture = sample_instructions_picture;
// h*0.8 * sample_instructions_picture.naturalWidth / sample_instructions_picture.naturalHeight; 
// make a short name for shuffle function
var shuffle = jsPsych.randomization.shuffle;

var x_rate=1000;
var initial_balance = 100;
var other_contribution_first_and_last = {
    0: { // neutral
        first: 0.55, last: 0.20
    },
    1: { // pos
        first: 0.55, last: 0.20
    },
    2: { // neg
        first: 0.55, last: 0.20
    }
};
var other_contribution_first = undefined;
var other_contribution_last = undefined;

var balance = initial_balance;
var contribution = 0;
var other_contribution = 0;
var other_contribution_fraction = 0;
var other_balance=initial_balance;
var other_wins=0;
var other_old_balance=0;
var other_balance_after_contribution=0;
var pot = 0;
var participant_wins = 0;
var old_balance = 0;
var balance_after_contribution = 0;
var contribution_error = "";
var framing_idx = undefined;
var round = 0;
var n_rounds = undefined;

/*** Text / durations to edit starts ***/

const EXPERIMENT_INSTRUCTIONS_HTML_1 = `
<p> <u>Instructions - Please read carefully!</u> </p>
<p> You will be participating in a simulated economy with another participant. You will be automatically and randomly matched. <b> No one's identity will be revealed at any point, but you will be able to see the choices you both make. </b> Both of you will be presented with the same series of choices. <b> Your bonus depends on the decisions you make and the decisions of the other participant.</b> </p>
`
const EXPERIMENT_INSTRUCTIONS_HTML_2 = `
<p> <u>Instructions - Please read carefully!</u> </p>
When you begin the simulation, you will be provided with 100 coins. In this simulated economy, tax is voluntary. You will be given a choice of how much, if any, of these coins you would like to pay in tax. Just as in the real world, taxes will be spent on things that benefit everyone. <strong>The success of these policies is returned to you both equally, no matter how much tax you paid. </strong> </p> 
<p>For example, the NHS is funded by taxes. Everyone can access the NHS no matter how much tax they contribute. So we all receive the benefits of healthcare, no matter how much tax we pay.</p>
`

const EXPERIMENT_INSTRUCTIONS_HTML_3 = `
<p> <u>Instructions - Please read carefully!</u> </p>
<b> Each round you will decide how much tax to pay. This money is deducted from your balance and contributed into the public pot. The other participant will do the same and the total is multiplied by 1.2. </b> The multiplier rate represents how everyone can benefit from these policies and makes society better off. </p>
<p>For example, the NHS provides free healthcare to everyone. This has many benefits; children spend less time sick and therefore more time in school. A better education means they can get a better job, live a longer life and be a better parent.</p> 
<p> In this way, the money we spend on the NHS generates more money for the economy than the initial investment (about 1.2 times bigger i.e. every £100 spent on the NHS will generate £120 in economic benefits for everyone). </p>
`
const EXPERIMENT_INSTRUCTIONS_HTML_3a =`
<p> <u>Instructions - Please read carefully!</u> </p>
<p> <b> Each round you pay tax towards a different policy. This policy will display for 10 seconds before you can make a decision. They are all equally successful and the policy has no effect on the structure of the task. </b> The pot will always be multiplied by 1.2, and will always be divided equally amongst you. </p>
<p> However, we do <b> encourage you to contribute more to the themes you support </b> as we will donate 10% of our research budget to a charity related to the most popular policy theme. The charities are related to the themes in that they work towards various carbon mitigation strategies, plastic pollution, renewable energy, deforestation, aid to those affected by climate change, green transport, wildlife protection, transition to green jobs and others. You should be aware that <b>the other participant has the same incentive.</b></p>
`

const EXPERIMENT_INSTRUCTIONS_HTML_3b =`
<p> <u>Instructions - Please read carefully!</u> </p>
<p>Once the public pot has been multiplied, it is split equally between you both, <b> no matter how much you contributed. </b> This is added to your total balance. The amount you contribute and how much money you made are public information. Your partner faces the exact same decision. <b>They can not see your balance. </p><p> You will repeat these decisions, until the simulation randomly stops.</b> </p>
`
const EXPERIMENT_INSTRUCTIONS_HTML_4 = `
<p> <u>Instructions - Please read carefully!</u> </p>
You will be given 100 coins to start the task. You can contribute as much or as little as you like. Remember: the amount of real money you earn from this task depends on your balance at the end of the simulation.</p>
You will be given a flat rate reward for participating in this task plus a bonus depending on how well you perform. At the end of the task your balance will be converted (at an exchange rate of 1000 coins: £1) to your bonus.</p>
<p> On the next page you will have to answer questions so that we can check you understand the task. Once you start these questions you can not come back to the instructions, so please take your time to understand them. You will not be able to progress untill you answer all the questions correctly.</p>
`

const CONSENT_INSTRUCTIONS_HTML = `
<p> <u>Information Sheet for Participants</u> </p>
<p> <b>Study title:</b> Information in Economic Games </p>
<p> <b>Principal Investigator:</b> Neil Bramley </p>
<p> <b>Researcher collecting data:</b> J. Wise </p>
<p> <b>What is this page?</b>: This document explains what kind of study we’re doing, what your rights are, and what will be done with your data. You should print this page for your records. </p>
<p> <b>Nature of the study.</b> You are invited to participate in a study which involves reading sentences carefully and playing a game. Once you finish, we may also have some questions about you (e.g., age, gender, approximate income band and current mood). Your responses will be recorded. Your session should last no more than 15 minutes. You will be given full instructions shortly.</p>
<p> <b>Compensation.</b> You will be paid a set participation fee plus some reward depending on your performance in the simulation.</p>
<p> <b>Risks and benefits.</b>  There are no known risks to participation in this study. Other than the payment mentioned, there are no tangible benefits to you, however you will be contributing to our knowledge about cooperation.  </p>
<p> <b>Confidentiality and use of data. </b> All the information we collect during the course of the research will be processed in accordance with Data Protection Law. In order to safeguard your privacy, we will never share personal information (like names or dates of birth) with anyone outside the research team; if you agree and want to be contacted for future studies, we will add your contact details to our secure participant database. Your data will be referred to by a unique participant number rather than by name. Please note that we will temporarily collect your worker ID to prevent repeat participation, however we will never share this information with anyone outside the research team. We will store any personal data (e.g., audio/video recordings, signed forms) using the University of Edinburgh’s secure encrypted storage service or in a locked filing cabinet at the University of Edinburgh. The anonymized data collected during this study will be used for research purposes. With your permission, identifiable data such as recordings may also be used for research or teaching purposes, and may be shared with other researchers or with the general public (e.g., we may make it available through the world wide web, or use it in TV or radio broadcasts).</p>
<p> <b>What are my data protection rights? </b> The University of Edinburgh is a Data Controller for the information you provide.  You have the right to access information held about you. Your right of access can be exercised in accordance Data Protection Law. You also have other rights including rights of correction, erasure and objection.  For more details, including the right to lodge a complaint with the Information Commissioner’s Office, please visit www.ico.org.uk.  Questions, comments and requests about your personal data can also be sent to the University Data Protection Officer at dpo@ed.ac.uk.</p>
<p> <b>Voluntary participation and right to withdraw. </b> Your participation is voluntary, and you may withdraw from the study at any time and for any reason. If you withdraw from the study during or after data gathering, we will delete your data and there is no penalty or loss of benefits to which you are otherwise entitled.</p>
<p> If you have any questions about what you’ve just read, please feel free to ask, or contact us later. You can contact us by email at s1725800@ed.ac.uk . This project has been approved by PPLS Ethics committee. If you have questions or comments regarding your own or your child’s rights as a participant, they can be contacted at 0131 650 4020 or ppls.ethics@ed.ac.uk.</p>
<p> By continuing, you consent to the following: </p>
<p> 1.	<b> I agree to participate in this study. </b> </p>
<p> 2.	I confirm that I have read and understood <b> how my data will be stored and used.</b> </p>
<p> 3.	I understand that I have the <b>right to terminate this session at any point</b>. If I choose to <b>withdraw up to a week after completing the study</b>, my data will be deleted on the provision of my ID number.</p>

`
const COMPREHENSION_CHECKS_HTML = `
<p>Before we begin, please confirm that you understand the rules of the simulation by answering a couple of questions.</p>
`

const MATCHING_HTML = `
<p>Please wait, matching you with another participant.</p><p> Average wait time is 2 minutes.</p> <p> Please contact the lab if it takes more than 10 minutes. </p>
<div class="wait"></div>
`

function matching_duration() {
    return Math.round(750 + Math.random() * 30000);
}

 const GET_CONTRIBUTION_HTML = `
<p>How many coins would you like to contribute to the public pot?</p>
` 

const GETTING_RESPONSE_HTML = `
<p>Please wait, getting the other participant's response.</p>
<div class="wait"></div>
`

function getting_response_duration() {
    return Math.round(500 + Math.random() * 1000);
}

//Giving the robot fixed numbers to contribute each round
    function other_contribution_fraction_const(round) {
        var fractions = [0.60, 0.58, 0.49, 0.55, 0.39, 0.43, 0.37, 0.41, 0.36, 0.31, 0.34, 0.23, 0.29, 0.1, 0.20, 0.24];
        return fractions[round];
    }
    
function check_contribution_and_calculate_return(data) {
    contribution = Number(data.response.contribution);
    data.contribution = contribution;
    if (Number.isNaN(contribution)) {
        contribution_error = "Please enter a number";
        return;
    } else if (contribution < 0) {
        contribution_error = "Please enter a number greater than zero";
        return;
    } else if (contribution > balance) {
        contribution_error = `Please enter a number less than your current balance (${balance})`;
        return;
    }
    contribution_error = "";
    old_balance = balance;
    balance_after_contribution = balance - contribution;

    /*This was making the other's contribution random between certain sensible values
    other_contribution = Math.round(Math.random()*initial_balance*0.2)*5; */
    /* Calculate other_contribution, this is linearly decreasing from the start
    other_contribution_fraction = (
        other_contribution_last +
        (other_contribution_first - other_contribution_last) * (n_rounds - 1 - round) / (n_rounds - 1)
    );
    */

    other_contribution = initial_balance * other_contribution_fraction_const(round);
    data.other_balance=other_balance;
    other_balance_after_contribution=balance-other_contribution;
    var contrib_total = contribution + other_contribution;
    pot = Math.round(contrib_total * 1.2);
    participant_wins = Math.round(pot * 0.5);
    balance = balance_after_contribution + participant_wins;
    other_balance=other_balance_after_contribution + participant_wins;
    data.round=round;
    round += 1;
    return balance, other_contribution, other_balance;
}

// this has to be a function instead of a constant because it has variables in!, it's not called atm

function calculation_explanation() {
    return `
<p>You made a contribution of ${contribution}</p>
<p>other_contribution_fraction: ${other_contribution_fraction}</p>
<p>The other participant made a contribution of ${other_contribution}</p>
<p>The pot was calculated (after multiplying by 1.2) as ${pot}</p>
<p>Your share was ${participant_wins}</p>
`;
}

function draw_down_arrow(ctx, x, y_start, y_end, width, colour) {
    const head_height = width * 0.5;
    const head_width = width;
    const shaft_width = width * 0.5;
    ctx.fillStyle = colour;
    ctx.beginPath();
    ctx.moveTo(x - shaft_width * 0.5, y_start);
    ctx.lineTo(x - shaft_width * 0.5, y_end - head_height);
    ctx.lineTo(x - head_width * 0.5, y_end - head_height);
    ctx.lineTo(x, y_end);
    ctx.lineTo(x + head_width * 0.5, y_end - head_height);
    ctx.lineTo(x + shaft_width * 0.5, y_end - head_height);
    ctx.lineTo(x + shaft_width * 0.5, y_start);
    ctx.closePath()
    ctx.fill();
    ctx.stroke();
}

function calculation_explanation_canvas(canvas) {
    const ctx = canvas.getContext('2d');
    const w = canvas.width + 50;
    const h = canvas.height;
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.strokeStyle = "black";
    ctx.lineWidth = 2;
    ctx.font = "bold 1rem sans";

    ctx.fillStyle = "black";

    // entire body replaced every trial, so create image element just
    // before we use it.

    var div = document.createElement("div");
    div.style = "display: none;"
    var imgp = document.createElement("img");
    imgp.src = PLAYER_IMAGE;
    div.appendChild(imgp);
    document.body.appendChild(div);

    var adjusted_imgp_w = h * 0.15 * imgp.naturalWidth / imgp.naturalHeight;
    var imgp_x = w * 0.3 - adjusted_imgp_w / 2;
    imgp.onload = function() {
        ctx.drawImage(imgp, imgp_x, 90, 0.8*adjusted_imgp_w, h * 0.15*0.8);
    }

    var div = document.createElement("div");
    div.style = "display: none;"
    var img = document.createElement("img");
    img.src = OTHER_PLAYER_IMAGE;
    div.appendChild(img);
    document.body.appendChild(div);

    var adjusted_img_w = h * 0.15 * img.naturalWidth / img.naturalHeight;
    var img_x = w * 0.7 - adjusted_img_w / 2;
    img.onload = function() {
        ctx.drawImage(img, img_x, 90, 0.8*adjusted_img_w, h * 0.15*0.8);
    }
    /* Debugging issues
    alert(framing_idx);
    alert(round-1);
    alert(all_framing[framing_idx, round-1]);
    alert(all_framing[framing_idx, round-1].split('<p>')[2]);
  */  
var first_line = all_framing[framing_idx, round-1].split("<p>")[2];

first_line = first_line.replace("</p>","");
var lines = [];
var current_line = "";
for (word of first_line.split(" ")) {
    current_line += word+" ";
    if (current_line.length > 50) {
        lines.push(current_line);
        current_line = "";
    }
}
lines.push(current_line);

    var line_pos_y = 0.05;
    for (var line of lines) {
        ctx.fillText(line, w * 0.5, h * line_pos_y);
        line_pos_y += 0.05;
    }

    var tempBalance = old_balance-contribution;
    ctx.fillText(`You had ${old_balance.toFixed(0)} coins, then you gave ${contribution.toFixed(0)}.`, w * 0.25, h * 0.33);
    ctx.fillText(`${old_balance.toFixed(0)} - ${contribution.toFixed(0)} = ${tempBalance.toFixed(0)}.`, w * 0.25, h * 0.38);
    ctx.fillText(`${OTHER_PLAYER_NAME} gave ${other_contribution.toFixed(0)} coins.`, w * 0.7, h * 0.35);

    draw_down_arrow(ctx, w * 0.3, h * 0.4, h * 0.5, w * 0.1, "green");
    draw_down_arrow(ctx, w * 0.7, h * 0.4, h * 0.5, w * 0.1, "blue");

    ctx.fillStyle = "yellow";
    ctx.fillRect(w * 0.2, h * 0.5, w * 0.6, h * 0.1);
    ctx.strokeRect(w * 0.2, h * 0.5, w * 0.6, h * 0.1);

    const total = contribution + other_contribution;
    ctx.fillStyle = "black";
    ctx.fillText(`Pot: ${total.toFixed(0)}`, w * 0.5, h * 0.55);

    draw_down_arrow(ctx, w * 0.5, h * 0.6, h * 0.7, w * 0.1, "purple");

    ctx.fillStyle = "black";
    ctx.fillText("x 1.2", w * 0.85, h * 0.65);

    ctx.fillStyle = "yellow";
    ctx.fillRect(w * 0.2, h * 0.7, w * 0.6, h * 0.1);
    ctx.strokeRect(w * 0.2, h * 0.7, w * 0.6, h * 0.1);

    ctx.fillStyle = "black";
    ctx.fillText(`Pot: ${pot.toFixed(0)}`, w * 0.5, h * 0.75);

    draw_down_arrow(ctx, w * 0.3, h * 0.8, h * 0.9, w * 0.1, "green");
    draw_down_arrow(ctx, w * 0.7, h * 0.8, h * 0.9, w * 0.1, "blue");

    var balance_after_move= participant_wins + tempBalance;
    ctx.fillStyle = "black";
    ctx.fillText(`You got ${participant_wins.toFixed(0)} coins. ${tempBalance.toFixed(0)}+${participant_wins.toFixed(0)}=${balance_after_move.toFixed(0)}.`, w * 0.3, h * 0.93);
    ctx.fillText(`Now you have ${balance_after_move.toFixed(0)} coins.`, w * 0.3, h * 0.98);
    ctx.fillText(`${OTHER_PLAYER_NAME} got ${participant_wins.toFixed(0)} coins.`, w * 0.7, h * 0.95);
}

function result_prompt() {
    // 0 neutral, 1 pos, 2 neg
    var feedback = ``;
    if (framing_idx == 0) {
        feedback = `You earned ` + participant_wins.toFixed(0) + ` coins.`;
    } else if (framing_idx == 1) {
        feedback = `If you had both given nothing, the minimum you would have earned was 0 coins. You earned ` + participant_wins.toFixed(0) + ` <strong>more</strong> than this.`;
    } else if (framing_idx == 2) {
        var max_possible_win = (((old_balance + other_contribution) * 1.2)*0.5);
        feedback = `If you had given your whole balance, the maximum you could have earned was ` + max_possible_win.toFixed(0) + ` coins. You earned ` + (max_possible_win - participant_wins).toFixed(0) + ` <strong>less</strong> than this.`;
    }
    return feedback;
}

function text_enable_disable(radio_name, radio_id, text_id) {
    // set up a text input to enable or disable when a certain radio button is
    // checked
    function radio_callback(ev) {
        var radio = document.getElementById(radio_id);
        var text = document.getElementById(text_id);
        // Text should be enabled (!disabled) when radio button is checked
        // If this is not the case, need to enable / disable text according
        // to radio button state
        // & extra logic for fields which must be required when enabled
        var required_if_enabled = (text.getAttribute("data-required-if-enabled") == "yes");
        if (text.disabled == radio.checked) {
            if (text.disabled) {
                text.disabled = false;
                if (required_if_enabled && !text.hasAttribute("required")) {
                    var attr = document.createAttribute("required");
                    text.setAttributeNode(attr);
                }
            } else {
                if (required_if_enabled && text.hasAttribute("required")) {
                    text.removeAttribute("required")
                }
                text.disabled = true;
            }
        }
    }
    document.querySelectorAll('[name="' + radio_name + '"]').forEach(
        function (el) { el.addEventListener("change", radio_callback); }
    );
    // assume initially unchecked
    var text = document.getElementById(text_id);
    text.disabled = true;
}

function stash_form_values(trial_data) {
    // add its values to the whole result as columns
    jsPsych.data.addProperties(trial_data.response);
}

const DEBRIEF_HTML = `
<p> Dear participant, </p>
<p> <strong>Thank you for participating in our experiment!</strong> </p>
<p> <strong>What was the experiment about? </strong></p>
<p> Sometimes what is good for an individual in the short term is not the same as what is good for them in the long term, or even what is good for society as a whole. This can create serious problems, such as climate change. Economists and Psychologists call these kinds of problems ‘Social Dilemmas’. Everyone has an incentive to avoid the unpleasant/tiring/boring task, but if everyone does this then we all suffer.</p>
<p> We wanted to find out if there are better or worse ways to frame social dilemmas to make people more likely to do what benefits everyone in the long run (including themselves). We were interested in whether emphasising the costs of a choice relative to the most immediately rewarding choice, vs emphasising the benefits of a choice relative to the least immediately rewarding choice, make people more or less likely to behave in ways that maximize collective goods in the long run. You may have noticed the text you read throughout the study highlighted either a positive or a negative alternative outcome (unless you were in the control group in which case there were no prompts about potential alternative outcomes). We plan to compare the amount contributed per turn in each condition and across each choice in this study to see if these differences in “framing” made a difference. </p>
<p> <strong>How did you do that?</strong></p>
<p> A common task in behavioural economics is the Public Goods Game (Tavoni et al, 2011). In a public goods game, two or more participants are given an initial stake of money and invited to repeatedly donate some portion of that pot to a shared pot, which is then multiplied by some number >1 and divided evenly among the players. If players succeed in co-operating, they should put large amounts of their own stake in the pot every time as this will result in a greater final total for everyone.  </p>
<p> However, a true public goods game lacks experimental control: For example, some participants will inevitably be partnered with others who are never willing to share their stake, or share all of their stake every time, making for complex and unpredictable outcomes. Therefore, we opted to simulate the behaviour of your partner. This means that while you were told you were playing with another human, this was not in fact the case. This choice allowed us to ensure that all participants faced the same situation. This helps us establish that any pattern we find in the data can only be due to the way the choices were framed, rather than who you were playing against. </p>
<p> The goal of this work is to contribute to scientific knowledge about how people make decisions relating to short- and long-term rewards. These results of this work may be applied to collective action problems in society such as climate change.  </p>
<p> Thank you for your participation. </p>
<p> If you have any questions, please get in touch with Neil Bramley (neil.bramley@ed.ac.uk).</p>
<p> Thank you!</p>
`

// Framing texts are all shuffled at source, so will appear in random order
var pos_framing = shuffle([
    `<p id='pollution_health'>Emissions  from burning fossil fuels, like oil and coal, pollute our air and contaminate our water.</p> <p> Switching to renewable energy could make people healthier, <strong> saving 40,000 lives each year.</strong></p><p>This tax will be spent on developing clean energy.</p>`,
    `<p id='ocean_acid'>Emissions from burning fossil fuels, like oil and coal, pollute our air and contaminate our water.</p> <p> Switching to renewable energy <strong>prevents further damage</strong> to our natural resources. The ocean was <strong>30% less acidic</strong> before we started burning coal.</p><p>This tax will be spent on restoring the ocean.</p>`,
    `<p id='nat_sec'>We depend heavily on oil imports from countries linked to extremist terrorism.</p><p> Alternative energy sources <strong>reduce</strong> our national security <strong>risk</strong>. This could <strong>save $81 billion</strong> on defence spending to protect oil imports (based on the U.S, 2017).</p><p> This tax will be spent on developing reliable energy sources.</p>`,
    `<p id='econ_warfare'>We depend heavily on gas imports. The European Union imports 60% of their fossil fuels.</p><p> Transitioning to renewables <strong>makes our country safer from economic warfare and more independent.</strong></p><p> This tax will be spent on developing more independent energy sources. </p>`,
    `<p id='business'>Many entrepreneurial companies are developing ways of making renewable energy.</p> <p> If we adopt and implement similar practices, this will <strong>create 24 million new jobs</strong> by 2030. </p> <p>This tax will be invested in creating green jobs </p>`,
    `<p id='comp_adv'>Foreign countries are starting to outcompete our renewable energy market. We can maintain this advantage by investing in our businesses.</p><p> For every £1 we invest in a renewable energy business, it generates £1.50, that's <strong>90p more </strong>than investing in a fossil fuel business.</p> <p>Government investment in renewable energy is better for business. </p>`,
    `<p id='heat_death'>Our current energy system is a threat to both present and future generations.</p><p> By 2050, up to 7,000 people could die every year due to heatwaves, compared to 2,000 today. Switching to renewable energy will <strong>save lives in the next generation</strong>. </p> <p>This tax will be spent on heat wave preparation. </p>`,
    `<p id='deforrest'>Between 2000 and 2009, 32 million acres of tropical rainforest were cut down.</p> <p> Eliminating deforestation will <strong>prevent</strong> the emission of 200 billion tons of carbon into the atmosphere in the coming decades and may <strong>slow or prevent the sixth mass extinction.</strong></p><p> This tax will be spent on increasing biodiversity.</p>`,
    `<p id='green_job_health'>Jobs in the fossil fuel industry are dangerous and don't typically pay well.</p><p> Green jobs could <strong>save 1,300 lives</strong> over a decade (U.S, 2009).</p> <p> This tax will be spent on helping those in the fossil fuel industry transition to green jobs.</p>`,
    `<p id='intersectional'>According to the UN, "Women represent a high percentage of poor communities that are highly dependent on local natural resources for their livelihood".</p><p>The effects of climate change will be felt the <strong>least by the rich, men and white people</strong>. Everyone deserves to live in a healthy community. </p><p>This tax will be donated to those severely affected by climate change. </p>`,
    `<p id='disease'>People who have always worked in the fossil fuel industry are worried about losing their livelihoods and way of life. We can protect their wellbeing by helping them transition to better paying and safer jobs in green technology.</p><p>Circulatory, respiratory, central nervous system, and musculoskeletal <strong>issues</strong>, as well as <strong>birth defects</strong> are <strong>significantly lower</strong> in non-coal-mining regions.</p> <p>This tax will be invested in green-technology businesses in coal-mining regions. </p>`,
    `<p id='plastic_pol'>Improper disposal of plastic pollutes our air and water.</p><p> Proper disposal of plastic could <strong>save 1 million seabirds</strong> a year.</p> <p>This tax will be spent on improving recycling facilities. </p>`,
    `<p id='sea_levels'>Ice in Antarctica is melting and will raise global sea levels by 0.4 to 0.8m by 2100, threatening many coastal areas.</p><p> If we reach net zero by 2030, these coastal areas may survive - <strong>providing beautiful habitats</strong>, and the <strong>economics benefits</strong> from tourists visiting these habitats.</p><p> This tax will be spent on carbon capture programs. </p>`,
    `<p id='cycle_health'>Cycle to work schemes encourage people to cycle rather than drive to work in order to reduce carbon emissions.</p><p> Cycling to work every day (compared to driving) <strong>reduces your risk of dying by 41%</strong> from all causes.</p> <p> This tax will be spent on cycle paths. </p>`,
    `<p id='com_garden'>Community gardens are used to grow affordable, nutritious and environmentally friendly food.</p><p> In areas with community gardens, <strong>85% felt it improved food security</strong> for their household compared to when there was no garden.</p><p> This tax will be spent on communnity garden programs</p>`,
    `<p id='green_econ_boost'>Green energy is better for the global economy than business-as-usual scenario.</p><p> Renewable energy could <strong>add $100tn (£80tn)</strong> to the global economy between now and 2050, compared with a business-as-usual scenario.</p> <p> This tax will be spent on developing green energy. </p>`
]);
var neg_framing = shuffle([
    `<p id='pollution_health'>Emissions from burning fossil fuels, like oil and coal, pollute our air and contaminate our water.</p><p> This makes people sick, <strong>killing 40,000 people each year.</strong></p> <p>This tax will be spent on developing less polluting energy.</p>`,
    `<p id='ocean_acid'>Emissions from burning fossil fuels, like oil and coal, pollute our air and contaminate our water.</p><p> This <strong>degrades</strong> our natural resources. The ocean has become <strong>30% more acidic</strong> since we started burning coal.</p><p>This tax will be spent on reducing ocean acidity.</p>`,
    `<p id='nat_sec'>We depend heavily on oil imports from countries linked to extremist terrorism.</p><p> This puts our national security at <strong>greater risk</strong>. This <strong>costs $81 billion</strong> on defence spending to protect oil imports (based on the U.S, 2017).</p><p> This tax will be spent on developing less dangerous energy sources</p>`,
    `<p id='econ_warfare'>We depend heavily on gas imports. The European Union imports 60% of their fossil fuels. This means other countries can sabotage our economy.</p><p> This leaves our country <strong>more vulnerable</strong> to economic warfare. </p><p> This tax will be spent on developing less vulnerable energy sources. </p>`,
    `<p id='business'>Many entrepreneurial companies are developing ways of making renewable energy.</p><p> If we do not adopt and implement similar practices we will <strong>lose the creation of 24 million new jobs</strong> by 2030.</p><p> Contributing to this tax will create green jobs. </p> `,
    `<p id='comp_adv'>Foreign countries are starting to outcompete our renewable energy market.</p><p> We can maintain this advantage by investing in these businesses. For every £1 we invest in a fossil fuel business, it generates 60p, that's <strong>90p less</strong> than investing in a renewable energy business.</p> <p>Government investment in fossil fuels is not as good for business. </p>`,
    `<p id='heat_death'>Our current energy system is a threat to both present and future generations.</p><p> Today, 2,000 people die per year due to heat, by 2050 this will <strong>rise</strong> to 7,000. Failure to switch to renewable energy will <strong>kill</strong> in the next generation.</p> </p> <p>This tax will be spent on heat wave adaption. </p>`,
    `<p id='deforrest'>Between 2000 and 2009, 32 million acres of tropical rainforest were cut down.</p><p> At that rate, deforestation will pollute 200 billion tons of carbon into the atmosphere in the coming decades and <strong>increases the rate of the sixth mass extinction.</strong></p><p> This tax will be spent on reducing biodiversity loss.</p>`,
    `<p id='green_job_health'>Jobs in the fossil fuel industry are dangerous and don't typically pay well compared to green jobs. </p><p>Over a decade <strong>1,300 workers will die</strong> in the fossil fuel industry (U.S, 2009). </p> <p> This tax will be spent on helping those in the fossil fuel industry transition to green jobs.</p>`,
    `<p id='intersectional'>According to the UN, "Women represent a high percentage of poor communities that are highly dependent on local natural resources for their livelihood".</p><p>The effects of climate change are likely to <strong>disproportionately harm the poor, women, and people of colour</strong>. Everyone deserves to live in a healthy community.</p> <p>This tax will be donated to those severely affected by climate change. </p>`,
    `<p id='disease'>People who have always worked in the fossil fuel industry are worried about losing their livelihoods and way of life. They are worried even though their wellbeing being is lower than if they worked in Green technology.</p><p>Circulatory, respiratory, central nervous system, and musculoskeletal <strong>issues</strong>, as well as <strong>birth defects</strong> are <strong>significantly higher</strong> in coal-mining regions.</p> <p>This tax will be invested in green-technology businesses in coal-mining regions. </p>`,
    `<p id='plastic_pol'>Improper disposal of plastic pollutes our air and water.</p><p> This <strong>kills 1 million seabirds</strong> a year.</p> <p>This tax will be spent on improving recycling facilities. </p>`,
    `<p id='sea_levels'>Ice in Antarctica is melting and will raise global sea levels by 0.4 to 0.8m by 2100, threatening many coastal areas.</p><p> If we don't reach net zero by 2030, these coastal areas will not survive - <strong>losing beautiful habitats</strong>, and the <strong>economics benefits</strong> from tourists visiting these habitats.</p> <p> This tax will be spent on carbon capture programs. </p>`,
    `<p id='cycle_health'>Cycle to work schemes encourage people to cycle rather than drive to work in order to reduce carbon emissions.</p><p> Driving to work every day (compared to cycling) <strong>increases your risk of dying by 41%</strong> from all causes.</p><p> This tax will be spent on cycle paths. </p>`,
    `<p id='com_garden'>Community gardens are used to grow affordable, nutritious and environmentally friendly food.</p><p> In areas that were given a community garden, <strong>15% said they felt no difference in food security</strong> compared to when there was no garden.</p> <p> This tax will be spent on community garden programs.</p>`,
    `<p id='green_econ_boost'>Business-as-usual is worse for the global economy than green energy.</p><p> Following a business-as-usual scenario means the global economy would have <strong>$100tn (£80tn) less </strong>compared to an Economy based on renewable energy.</p> <p> This tax will be spent on developing green energy. </p>`
]);
var neu_framing = shuffle([
    `<p id='pollution_health'>Emissions from burning fossil fuels, like oil and coal, pollute our air and contaminate our water.</p><p> This affects our population's wellbeing and is linked to 40,000 deaths each year.</p><p>This tax will be spent on developing renewable energy.</p>`,
    `<p id='ocean_acid'>Emissions from burning fossil fuels, like oil and coal, pollute our air and contaminate our water.</p><p> This affects our natural resources, such as the acidity of the ocean.</p><p>This tax will be spent on reducing ocean acidity.</p>`,
    `<p id='nat_sec'>We depend heavily on oil imports from countries linked to extremist terrorism.</p><p> This has implications for our national security. In 2017, the US spent approximately $81 billion on defence spending for oil imports.</p> <p> This tax will be spent on developing alternative energy sources</p>`,
    `<p id='econ_warfare'>We depend heavily on gas imports. This means other countries could influence our economy.</p><p> The European Union produces 40% of the fossil fuels it consumes and imports the remaining 60%.</p> <p> This tax will be spent on developing alternative energy sources. </p>`,
    `<p id='business'>Many entrepreneurial companies are developing ways of making renewable energy.</p><p> The adoption of sustainable practices is estimated to employ 24 million jobs by 2030.</p> <p>This tax will be invested in these businesses. </p>`,
    `<p id='comp_adv'>Foreign countries are starting to outcompete our renewable energy market. We can maintain this advantage by investing in these businesses.</p><p> When you invest £1 in a fossil fuel business, it generates 60p. When you invest £1 in a renewable energy business, it generates £1.50.</p> <p> This tax will be invested in green businesses. </p>`,
    `<p id='heat_death'>Our current energy system is a threat to both present and future generations.</p><p> By 2050, up to 7,000 people could die every year due to heat.</p> </p> <p>This tax will be spent on heat wave mitigation. </p>`,
    `<p id='deforrest'>Between 2000 and 2009, 32 million acres of tropical rainforest were cut down.</p><p> At that rate, deforestation will release 200 billion tons of carbon into the atmosphere in the coming decades and has a significant impact on the sixth mass extinction.</p> <p> This tax will be spent on maintaining biodiversity.</p>`,
    `<p id='green_job_health'>Jobs in the fossil fuel industry have more accidents and don't typically pay as well as Green jobs. </p><p> </p> <p> This tax will be spent on helping those in the fossil fuel industry transition to green jobs.</p>`,
    `<p id='intersectional'>According to the UN, "Women represent a high percentage of poor communities that are highly dependent on local natural resources for their livelihood".</p><p>The effects of climate change will not be felt equally across the population.  Everyone deserves to live in a healthy community. </p> <p>This tax will be donated to those affected by climate change. </p>`,
    `<p id='disease'>People who have always worked in the fossil fuel industry are worried about losing their livelihoods and way of life.</p><p> Circulatory, respiratory, central nervous system, and musculoskeletal issues, as well as birth defects are associated with coal-mining regions. We can ease their transition to green jobs.</p> <p>This tax will be invested in green-technology businesses in coal-mining regions. </p>`,
    `<p id='plastic_pol'>Improper disposal of plastic pollutes our air and water.</p><p>  This affects 800 species worldwide.</p> <p>This tax will be spent on improving recycling facilities. </p> `,
    `<p id='sea_levels'>Ice in Antarctica is melting and will raise global sea levels by 0.4 to 0.8m by 2100, threatening many coastal areas.</p><p> This may flood many coastal areas which provide wildlife beautiful habitats and the economics benefits from tourists visiting these habitats.</p> <p> This tax will be spent on carbon capture programs. </p>`,
    `<p id='cycle_health'>Cycle to work schemes encourage people to cycle rather than drive to work in order to reduce carbon emissions.</p><p> A more active lifestyle is associated with better health.</p> <p> This tax will be spent on cycle paths. </p>`,
    `<p id='com_garden'>Community gardens are used to grow affordable, nutritious and environmentally friendly food.</p><p> 15% said that community gardens had no effect on their food security and 85% said it had an effect.</p> <p> This tax will be spent on community garden programs.</p>`,
    `<p id='green_econ_boost'>Renewable energy is better for the economy than business as usual.</p><p> The International Renewable Energy Agency estimates a $100tn (£80tn) difference.</p> <p> This tax will be spent on developing green energy. </p>`
]);



/*** Text / durations to edit ends ***/

// Select positive, negative or neutral framing
framing_idx = Math.floor(Math.random() * 3);
var all_framing = [neu_framing, pos_framing, neg_framing][framing_idx];
var n_rounds = all_framing.length;
other_contribution_first = other_contribution_first_and_last[framing_idx].first;
other_contribution_last = other_contribution_first_and_last[framing_idx].last;

/* create timeline */
var timeline = [];
timeline.push(preload);

/*
const PILOT_PREAMBLE_HTML=`<p>Hi Testers! Thank you so much for helping me test my experiment! I really appreciate it <3. </p>
<p>This is just a trial run for me to guage any issues with my experiment before I make it public. I won't acutally use this data, except to justify my sample size </p>
<p>Please keep going until you get to the feedback screen at the very end and make sure to hit enter... or else I lose your data :'(. It would also be helpful if you could time, roughly, how long this takes! </p>
      `


var pilot_preamble={
    type: jsPsychHtmlButtonResponse,
    stimulus: PILOT_PREAMBLE_HTML,
    choices: ["Cool, I shall keep an eye out for typos, and time myself"]
}
timeline.push(pilot_preamble);
*/

var consent = {
    type: jsPsychHtmlButtonResponse,
    stimulus: CONSENT_INSTRUCTIONS_HTML,
    choices: ["I consent and would like to continue"]
};
timeline.push(consent);

var instructions = {
    type: jsPsychInstructions,
    pages: [
        EXPERIMENT_INSTRUCTIONS_HTML_1,
        EXPERIMENT_INSTRUCTIONS_HTML_2,
        EXPERIMENT_INSTRUCTIONS_HTML_3,
        EXPERIMENT_INSTRUCTIONS_HTML_3a,
        EXPERIMENT_INSTRUCTIONS_HTML_3b,
        `<p>Here is an example of what the simulation will look like... </p> <img src=${adj_sample_instructions_picture} height=500 width=800>`,
        EXPERIMENT_INSTRUCTIONS_HTML_4,
    ],
    show_clickable_nav: true,
    data: {type: 'instructions'}
}
timeline.push(instructions);

var ConceptQuestions = ["Q1. What is your balance based on?","Q2. Why does your final balance matter?", "Q3. How does the simulation work?", "Q4. How does the theme matter?"]
var GameplayQuestions = ["Q5. How is the communal pot split?", "Q6. When does the simulation end?", "Q7. What information is revealed?"]
var ConceptOptions = [
    ["My decision", "My opponent\'s decision", "Both", "Neither"],
    ["It doesn't", "For my pride", "I will win a set fee if my balance is bigger than my opponent's at the end of the simulation", "At the end of the simulation, I will receive a bonus payment according to my balance"],
    ["Our contributions are added and split between us", "I receive whatver my partner contributes x1.2", "Our contributions are summed, multiplied by 1.2, and divided equally", "The mechanism changes throughout the simulation" ],
    ["I should contribute more to themes I support, becuase the most popular themes will receive a donation to a related charity", "The theme of the round has no effect on the simulation's structure i.e. how the money is multiplied and split", "Option 1 and 2 are both true", "Depending on the theme, it may increase my earnings", "Depending on the theme, it may decrease my earnings"]
]
var GameplayOptions = [
    ["Equally", "In proportion to contributions", "2/3 to me", "The pot is not split"],
    ["After 1 turn", "After 5 turns", "You didn't specify how many rounds there are"],
    ["We can both see what we contributed that round", "Our names, faces and addresses will be revealed"]
]
var ConceptAnswers = ["Both", "At the end of the simulation, I will receive a bonus payment according to my balance", "Our contributions are summed, multiplied by 1.2, and divided equally", "Option 1 and 2 are both true"]
var GameplayAnswers = ["Equally", "You didn't specify how many rounds there are", "We can both see what we contributed that round"]

var compQestionCount = ConceptQuestions.length + GameplayQuestions.length
var wrongQuestions= []

var comprehensionConceptQs = {
    type: jsPsychSurveyMultiChoice,
    preamble: COMPREHENSION_CHECKS_HTML,
    questions: [
      {
        prompt: ConceptQuestions[0],
        name: 'ConceptCheck0', 
        options: ConceptOptions[0],
        required: true,
        randomize_question_order: false, 
      }, 
      {
        prompt: ConceptQuestions[1],
        name: 'ConceptCheck1', 
        options: ConceptOptions[1],
        required: true,
        randomize_question_order: false, 
      }, 
      {
        prompt: ConceptQuestions[2],
        name: 'ConceptCheck2', 
        options: ConceptOptions[2],
        required: true,
        randomize_question_order: false, 
      },
      {
        prompt: ConceptQuestions[3],
        name: 'ConceptCheck3', 
        options: ConceptOptions[3],
        required: true,
        randomize_question_order: false, 
      }
    ],
    on_finish: function (data){
        wrongQuestions=[]
        var correctCount = 0;
        if (data.response.ConceptCheck0 == ConceptAnswers[0]) {
            correctCount += 1; 
        }
        if (data.response.ConceptCheck0 != ConceptAnswers[0]) {
            wrongQuestions.push( "1"); 
        }
        if (data.response.ConceptCheck1 == ConceptAnswers[1]) {
            correctCount +=1;
        }
        if (data.response.ConceptCheck1 != ConceptAnswers[1]) {
            wrongQuestions.push( "2"); 
        }
        if (data.response.ConceptCheck2 == ConceptAnswers[2]) {
            correctCount +=1;
        }
        if (data.response.ConceptCheck2 != ConceptAnswers[2]) {
            wrongQuestions.push( "3"); 
        }
        if (data.response.ConceptCheck3 == ConceptAnswers[3]) {
                correctCount +=1;
        }
        if (data.response.ConceptCheck3 != ConceptAnswers[3]) {
            wrongQuestions.push( "4"); 
        }
        data.correctCount = correctCount;
      }
  };

var comprehensionGameplayQs = {
    type: jsPsychSurveyMultiChoice,
    preamble: COMPREHENSION_CHECKS_HTML,
    questions: [
      {
        prompt: GameplayQuestions[0],
        name: 'GameplayCheck0', 
        options: GameplayOptions[0],
        required: true,
        randomize_question_order: false, 
      }, 
      {
        prompt: GameplayQuestions[1],
        name: 'GameplayCheck1', 
        options: GameplayOptions[1],
        required: true,
        randomize_question_order: false, 
      }, 
      {
        prompt: GameplayQuestions[2],
        name: 'GameplayCheck2', 
        options: GameplayOptions[2],
        required: true,
        randomize_question_order: false, 
      }
    ],
    on_finish: function (data){
        var correctCount = 0;
        if (data.response.GameplayCheck0 == GameplayAnswers[0]) {
            correctCount += 1; 
        }
        if (data.response.GameplayCheck0 != GameplayAnswers[0]) {
            wrongQuestions.push( "5"); 
        }
        if (data.response.GameplayCheck1 == GameplayAnswers[1]) {
            correctCount +=1;
        }
        if (data.response.GameplayCheck1 != GameplayAnswers[1]) {
                wrongQuestions.push( "6");
        }
        if (data.response.GameplayCheck2 == GameplayAnswers[2]) {
            correctCount +=1;
        }
        if (data.response.GameplayCheck2 != GameplayAnswers[2]) {
            wrongQuestions.push( "7");
        }
        data.correctCount = correctCount;
      }
  };

 var comprehensionFeedback = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function() {
        var cor = jsPsych.data.get().last(1).values()[0].correctCount + jsPsych.data.get().last(2).values()[0].correctCount;
        if (cor != compQestionCount) {
            return "<p> Sorry, you got " + (compQestionCount-cor).toString() + " wrong. <b>Retry question(s) " + (wrongQuestions).toString()+ ".</b> You can try again or leave the experiment and return to Prolific.</p>";
        } else {
            return "<p>All correct! Let's get started. If you'd rather not continue you can always leave the experiment and return to Prolific.</p>";
        }
    },
    choices: function () {
        var cor = jsPsych.data.get().last(1).values()[0].correctCount + jsPsych.data.get().last(2).values()[0].correctCount;
        if (cor != compQestionCount) {
            return ['Try Again', 'Leave Experiment'];
        } else {
            return ['Begin', 'Leave Experiment']; 
        }
    },
    on_finish: function(data) {
        if (data.response == 1) {
            go_back_to_prolific = true;
            jsPsych.endExperiment("Thank you for participating.");
        }
    }
};

var comprehension = [comprehensionConceptQs, comprehensionGameplayQs, comprehensionFeedback];

var compNode = {
    timeline: comprehension,
    loop_function: function() {
        if (jsPsych.data.get().last(2).values()[0].correctCount + jsPsych.data.get().last(3).values()[0].correctCount== compQestionCount) {
            return false;
        } else {
            return true;
        }
    }
};
timeline.push(compNode);

var wait = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: MATCHING_HTML,
    trial_duration: matching_duration,
    response_ends_trial: false
}
timeline.push(wait);

//This is from the variable below, when they could just click through and forced to wait
//type: jsPsychHtmlButtonResponse,
        //stimulus: framing_html,
        //choices: ["Got it!"]

for (var framing_html of all_framing) {
    var framing = {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: framing_html,
        trial_duration: 10000,
        response_ends_trial: false
    };
    timeline.push(framing);

    /* get response / contribution */
    var get_contribution = {
        type: jsPsychSurveyText,
        questions: [{
            prompt: function () {
                var prompt_text = "";
                if (contribution_error !== "") {
                    prompt_text = `<p>${contribution_error}</p> <p> How much would you like to contribute? Your current balance is: ${balance} coins </p>`;
                }
                else { 
                    prompt_text = `${all_framing[framing_idx,round]} How much would you like to contribute? Your current balance is: ${balance} coins`;
                }                
                return prompt_text;
            },
            columns: 3,
            rows: 1,
            required: true,
            name: "contribution"
        }],
        data: {
            current_balance: function () { return balance; },
        },
        on_finish: check_contribution_and_calculate_return
    };

    var validation_loop = {
        timeline: [get_contribution],
        loop_function: function (data) {
            return contribution_error !== "";
        }
    };
    timeline.push(validation_loop);

    var wait2 = {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: GETTING_RESPONSE_HTML,
        trial_duration: getting_response_duration,
        response_ends_trial: false
    }
    timeline.push(wait2);

    /*//For de-bugging, just checking calculations are as they should be
    var result = {
        type: jsPsychHtmlButtonResponse,
        stimulus: calculation_explanation,
        choices: ["Ok, on to the next round!"]
    }
    timeline.push(result);
    */

    var result_canvas = {
        type: jsPsychCanvasButtonResponse,
        stimulus: calculation_explanation_canvas,
        choices: ["Ok, on to the next round!"],
        prompt: result_prompt
    }
    timeline.push(result_canvas);
}

var demographics = {
    type: jsPsychSurveyHtmlForm,
    html: DEMOGRAPHICS_HTML,
    on_load: function () {
        text_enable_disable("gender", "selfidentify", "selfidentifiedgender");
    },
    on_finish: stash_form_values
}
timeline.push(demographics);

var likert_scale = [
    'I don\'t ever think about this or feel this way',
    'A few times a year', 
    'Around once a month', 
    'Several times a month', 
    'About once a week', 
    'Nearly every day'
  ];

  var MBI = {
    type: jsPsychSurveyLikert,
    preamble: `<p>These questions ask you how often you feel "X" <b>when</b> you think about "Y". </p>
    <p>For example, the first question asks how often you feel emotionally exhausted <b> when </b> you're thinking about the biggest challenges of the 21st century. This is <b>not</b> asking about how often you think about these challenges, but <b> how often you feel exhauseted</b> when you <b>do</b> think about them. </p>
    <p><b>Please estimate how often you identify with the following statements.</b></p>`,
    questions: [
      {prompt: "When I think about the biggest challenges of the 21st century, I feel emotionally exhausted.", name: 'EmotionalExhaustion', labels: likert_scale, required: true},
      {prompt: "I feel worn out at the end of a day.", name: 'TiredPM', labels: likert_scale, required: true},
      {prompt: "I feel guilty when I'm not doing something to improve human rights or social justice.", name: 'Guilt', labels: likert_scale, required: true },
      {prompt: "I feel burnt out because of the state of the world.", name: 'BurnedOut', labels: likert_scale, required: true},
      {prompt: "When I think about the injustices in the world, I feel hopeless.", name: 'Hopeless', labels: likert_scale, required: true},
      {prompt: "I feel hopeful that social change is possible.", name: 'ChangePossible', labels: likert_scale, required: true},
      {prompt: "I value and respect all life. I care about people I am not related to, have never met, nor will ever meet. ", name: 'Care_for_Life', labels: likert_scale, required: true},
      {prompt: "When I learn about current affairs I feel as if I'm at my wits' end.", name: 'WitsEnd', labels: likert_scale, required: true}
    ],
    randomize_question_order: false,
  };
  timeline.push(MBI);

var debrief = {
    type: jsPsychHtmlButtonResponse,
    stimulus: DEBRIEF_HTML,
    choices: ["I understand, bye!"],
}
timeline.push(debrief);

/*
//For feedback before making public
var pilot = {
    type: jsPsychSurveyText,
    preamble: `Thanks for helping me pilot! You're a star! Please give me some feedback so I can improve my study :)`,
    questions: [
      {prompt: 'If you have any thoughts would you let me know below e.g did you spot any typos? Were the instructions clear? Did you guess what the experiment was about? Could you tell it was not a real participant? Roughly, how long do you think it took?</p>', rows: 5}
    ]
  }
timeline.push(pilot);
*/

function save_data(name, data_in) {
    var url = 'save_data.php';
    var data_to_send = { filename: name, filedata: data_in };
    fetch(url, {
        method: 'POST',
        body: JSON.stringify(data_to_send),
        headers: new Headers({
            'Content-Type': 'application/json'
        })
    });
}

/* start the experiment */
jsPsych.run(timeline);
